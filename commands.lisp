(in-package :itbot)

(defcommand (:issue :i) (args source description)
  "project assign-to [type [version [category [priority]]]] \"subject\":text -- create an issue in Redmine"
  (when (< (length args) 3)
    (bot-error "bad issue spec"))
  (destructuring-bind (project assigned-to &optional tracker version category priority)
      (butlast args)
    (let* ((subject (lastcar args))
           (project (project project))
           (issue (post-issue project
                              :impersonate source
                              :subject subject
                              :description description
                              :assigned-to
                              (if (string= "me" assigned-to) source assigned-to)
                              :version (unless (string= version "-") version)
                              :category (unless (string= category "-") category)
                              :tracker (unless (string= tracker "-") tracker)
                              :priority (unless (string= priority "-") priority))))
      (format nil "New issue #~a '~a' created in project '~a': ~a"
              (object-id issue)
              (getf issue :subject)
              (getf project :name)
              (issue-url issue)))))

(defcommand (:status :s) (args source)
  "issue status -- set Redmine issue status"
  (destructuring-bind (&optional issue status) args
    (update-issue (or issue (bot-error "issue not specified"))
                  :impersonate source
                  :status (or status (bot-error "status not specified")))
    (format nil "Updated issue #~a: status -> ~a: ~a"
            (object-id issue)
            status
            (issue-url (object-id issue)))))

(defcommand (:versions :vs) (args source)
  "project -- display project versions (non-closed)"
  (unless (length= args 1)
    (bot-error "project not specified"))
  (iter (for version in (getf (project (first args)) :versions))
        (when (string= "open" (getf version :status))
          (collect
              (format nil "Version '~a'~@[: ~a~]~@[ (due: ~a)~]"
                      (getf version :name)
                      (null-if-empty (getf version :description))
                      (getf version :due-date))))))

(defcommand (:categories :cats) (args source)
  "project -- display project issue categories"
  (unless (length= args 1)
    (bot-error "project not specified"))
  (format nil
          "~{~a~^, ~}"
          (sort
           (iter (for category in (getf (project (first args)) :categories))
                 (collect (getf category :name)))
           #'string<)))

(defcommand :recent (args source)
  "[[limit] user] -- list recent issues (default limit 10)"
  (unless (<= 0 (length args) 2)
    (bot-error "bad arguments"))
  (destructuring-bind (&optional (limit "10") user)
      args
    (unless (or (null limit)
                (cl-ppcre:scan "^(-|\\d{1,20})$" limit))
      (bot-error "invalid limit spec"))
    (setf limit (if (and limit (not (string= "-" limit)))
                    (parse-integer limit)
                    10))
    (iter (for issue in (sort
                         (recent-issues :impersonate source :limit limit :user user)
                         #'<
                         :key #'(lambda (issue)
                                  (getf (getf issue :status) :id))))
          (collect
              (format nil "#~a (~a) ~a -- ~a (updated on ~a)"
                      (object-id issue)
                      (getf (getf issue :status) :name)
                      (getf issue :subject)
                      (issue-url issue)
                      (local-time:format-rfc1123-timestring
                       nil (local-time:parse-timestring (getf issue :updated-on))))))))

(defcommand :reload (args source)
  "-- reload Redmine data"
  (load-redmine-data)
  "Reloaded")

(defcommand :beaver (args source)
  "-- show beaver-related greeting"
  "И вам бобра")

(defcommand :summon (args source description)
  "to [\"title\"]: text -- send push notification to user's mobile device"
  (unless (<= 1 (length args) 2)
    (bot-error "bad summon spec"))
  (destructuring-bind (to &optional (title "IRC notification")) args
    (pushbullet-send-notification
     to (format nil "itbot: ~a summons you~@[: ~a~]" source title) description)
    (format nil "Sent push notification to ~a" to)))

(defcommand :reloadrc (args source)
  "-- reload rc files"
  (iter (for (path . error) in (load-rc-scripts))
        (collect
            (format nil "Loading ~a: ~a" (file-namestring path)
                    (if error (format nil "ERROR: ~a" error) "ok")))
        (else (return "No rc files"))))

(defcommand :help (args source)
  "-- show this help"
  (mapcar #'irc-command-doc (list-commands)))

(define-regex-handler task-number (source num) "#(\\d{1,20})"
  (setf num (parse-integer num))
  (let ((issue (issue-info num)))
    (format nil "Project ~a: ~a #~d: '~a' (~a) created~@[ by ~a~] on ~a~@[, assigned to ~a~]"
            (getf (getf issue :project) :name)
            (getf (getf issue :tracker) :name)
            num
            (getf issue :subject)
            (getf (getf issue :status) :name)
            (getf (getf issue :author) :name)
            (local-time:format-rfc1123-timestring
             nil (local-time:parse-timestring (getf issue :created-on)))
            (getf (getf issue :assigned-to) :name))))

(define-regex-handler pres (source) ("\\bп[а-я]*т[а-я]*н\\b.*\\bхуйло\\b" :case-insensitive-mode t)
  "ла-ла-ла-ла-ла-ла-ла")

(define-regex-handler huy (source) ("\\bхуй\\b" :case-insensitive-mode t)
  (format nil "~a: в моём присутствии попрошу не выражаться!" source))

;; TBD: listing issues w/query
