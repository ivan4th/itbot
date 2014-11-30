(in-package :itbot)

(defvar *connection* nil)
(defvar *thread* nil)
(defvar *nickname*)
(defvar *known-commands* '())
(defvar *regexp-handlers* '())
(defvar *bot-startup-func* nil)
(defvar *botrc-dir* nil)
(defvar *reconnect-wait-seconds* 5)

(defun tokenize-command (text)
  (let ((desc nil))
    (values
     (iter (until (emptyp text))
           (rx-match-case text
             ("^\\s+(.*)" (rest) (setf text rest))
             ("^'(.*?)'(.*)" (item rest) (collect item) (setf text rest))
             ("^\"(.*?)\"(.*)" (item rest) (collect item) (setf text rest))
             ("^:\\s*(.*)" (item) (setf desc item) (setf text ""))
             ("^([^:\\s]+)(.*)" (item rest) (collect item) (setf text rest))))
     desc)))

(defun parse-command (text)
  (multiple-value-bind (items desc)
      (tokenize-command text)
    (unless items
      (bot-error "bad command: ~a (use help command to get help)" text))
    (values (or (find-symbol (string-upcase (substitute #\- #\_ (first items))) :keyword)
                (bot-error "bad command: ~a (use help command to get help)" text))
            (rest items)
            desc)))

(defstruct (irc-command
            (:constructor make-irc-command (names desc-arg-p doc handler))
            (:type list))
  names desc-arg-p doc handler)

(defmacro defcommand (names (args &optional source-arg desc-arg) &body body)
  (with-gensyms (cmd)
    (setf names (ensure-list names))
    (let ((function-name (symbolicate 'command-handler-for- (first names)))
          (act-source-arg (or source-arg (gensym "SOURCE-ARG")))
          (act-desc-arg (or desc-arg (gensym "DESC-ARG")))
          (doc (format nil "~{~a~^|~}~@[ ~a~]"
                       (mapcar #'string-downcase names)
                       (when (stringp (first body))
                         (pop body)))))
      `(progn
         (defun ,function-name (,args ,act-source-arg ,act-desc-arg)
           (declare (ignorable ,args ,act-source-arg)
                    ,@(unless desc-arg `((ignore ,act-desc-arg)))
                    ,@(unless source-arg `((ignore ,act-source-arg))))
           ,@body)
         (let ((,cmd (make-irc-command ',names ,(when desc-arg t) ,doc ',function-name)))
           ,@(iter (for name in names)
                   (collect `(setf (get ',name 'irc-command) ,cmd)))
           (pushnew ',(first names) *known-commands*))))))

(defun list-commands ()
  (iter (for cmd in (sort *known-commands* #'string<))
        (collect (get cmd 'irc-command))))

(defmacro define-regex-handler (name (source &rest binds) regex &body body)
  (with-gensyms (text)
    (let ((func-name (symbolicate name '-regex-handler)))
      (destructuring-bind (rx &rest rx-options) (ensure-list regex)
        `(progn
           (defun ,func-name (,text ,source)
             (declare (ignorable ,source))
             (with-match ,binds (,rx ,text ,@rx-options)
               ,@body))
           (pushnew ',func-name *regexp-handlers*))))))

(defun handle-regex (text source)
  (iter (for handler in *regexp-handlers*)
        (thereis (funcall handler text source))))

(defun handle-command (text source dest)
  (when-let ((text (if (string= dest *nickname*)
                       text
                       (maybe-message-text text))))
    (multiple-value-bind (command-name args desc)
        (parse-command text)
      (let ((command (or (get command-name 'irc-command)
                         (bot-error "bad command: ~a (use help command to get help)" text))))
        (if (irc-command-desc-arg-p command)
            (unless desc (bot-error "desc expected for command ~a" command-name))
            (when desc (bot-error "no desc expected for command ~a" command-name)))
        (funcall (irc-command-handler command) args source desc)))))

(defun maybe-message-text (text &optional (to *nickname*))
  (iter (for join-with in '(":" "," " "))
        (let ((prefix (concat to join-with)))
          (when (starts-with-subseq prefix text)
            (return (null-if-empty (trim (subseq text (length prefix)))))))))

(defun invoke-command-handling-errors (text source dest)
  (handler-case
      (restart-case
          (let ((result (or (handle-command text source dest)
                            (handle-regex text source))))
            (cond ((null result) '())
                  ((stringp result)
                   (list result))
                  ((and (proper-list-p result)
                        (every #'stringp result))
                   result)
                  (t (list "ok"))))
        (skip-irc-command ()
          :report "Skip IRC command"
          (list "Lisp error!")))
    (bot-error (condition)
      (list (format nil "ERROR: ~a" condition)))))

(defun msg-hook (message)
  (let ((dest (if (string-equal (first (irc:arguments message)) *nickname*)
                  (irc:source message)
                  (first (irc:arguments message))))
        (text (lastcar (irc:arguments message))))
    (when text
      (dolist (line (invoke-command-handling-errors
                     text (irc:source message)
                     (first (irc:arguments message))))
        (irc:privmsg *connection* dest line)))))

(defparameter *thread-vars*
  '(*debug-io* *error-output* *query-io* *standard-input* *standard-output*
    *trace-output*))

;; Fix WTF (endless loop) in CL-IRC
(defmethod irc::read-irc-message :around ((connection irc:connection))
  (or (call-next-method)
      (error 'end-of-file :stream (irc:network-stream $c))))

(defun itbot-worker (nick server channels &key (port :default) (connection-security :none) (password nil))
  (setf *nickname* nick
        *connection* (irc:connect :nickname nick
                                  :server server
                                  :port port
                                  :password password
                                  :connection-security connection-security))
  (irc:add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (labels ((setup (message)
             (declare (ignore message))
             (dbg "-- setup --")
             (irc:mode *connection* *nickname* "+B") ;; bot mode
             (dolist (channel (ensure-list channels))
               (irc:join *connection* channel))
             (irc:remove-hook *connection* 'irc::irc-rpl_globalusers-message #'setup)))
    (irc:add-hook *connection* 'irc::irc-rpl_globalusers-message #'setup)
    (irc:read-message-loop *connection*)
    (dbg "*** Connection terminated.")))

(defun itbot-outer-worker (&rest args)
  (dbg "*** Loading Redmine data...")
  (load-redmine-data)
  (loop
    (handler-case
        (apply #'itbot-worker args)
      (usocket:socket-error (c)
        (dbg "*** Disconnecting due to socket error: ~a" c)))
    (dbg "*** Waiting for ~a seconds" *reconnect-wait-seconds*)
    (sleep *reconnect-wait-seconds*)
    (dbg "*** Reconnecting...")))

(defun start-itbot (&rest args)
  (setf *thread*
        (bt:make-thread
         #'(lambda () (apply #'itbot-outer-worker args))
         :name "IRC thread"
         :initial-bindings
         (mapcar #'cons
                 *thread-vars*
                 (mapcar #'symbol-value *thread-vars*)))))

(defun stop-itbot ()
  (when *thread*
    ;; FIXME: that's not quite correct way to do it
    (bt:destroy-thread *thread*)
    (iter (while (bt:thread-alive-p *thread*)))
    ;; FIXME
    (ignore-errors (irc:quit *connection*))
    (setf *connection* nil *thread* nil)))

(defun load-rc-scripts (&optional botrc-dir)
  (if botrc-dir
      (setf *botrc-dir* botrc-dir)
      (setf botrc-dir *botrc-dir*))
  (when botrc-dir
    (let ((script-paths (directory
                         (merge-pathnames
                          "*.lisp" (cl-fad:pathname-as-directory botrc-dir)))))
      (iter (for path in script-paths)
            (dbg "loading: ~s" path)
            (handler-case
                (progn
                  (load path)
                  (collect (cons path nil)))
              (error (c)
                (warn "error loading rc file ~a: ~a" path c)
                (collect (cons path c))))))))

(defun startup (&key botrc-dir)
  (load-rc-scripts botrc-dir)
  (unless *bot-startup-func*
    (error "*bot-startup-func* not defined!"))
  (funcall *bot-startup-func*))

#+sbcl
(defun main ()
  (handler-case
      (let ((args (rest sb-ext:*posix-argv*)))
        (handler-case
            (startup :botrc-dir (first args))
          (error (err)
            (format *error-output* "FATAL: error signalled: ~a~%" err)
            (sb-ext:exit :code 1)))
        (sb-impl::toplevel-repl nil))
    (sb-sys:interactive-interrupt ()
      (format t "Exiting due to an interactive interrupt.~%")
      (sb-ext:exit :code 1))))

#+sbcl
(defun save-image ()
  (sb-ext:save-lisp-and-die "itbot"
                            :executable t
                            :toplevel #'main))

#++
(start-itbot "itbot" "localhost" "#it")
