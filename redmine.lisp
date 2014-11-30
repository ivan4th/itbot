(in-package :itbot)

;; FIXME: define redmine-error

(define-error bot-error (simple-error) ())

(defvar *users* '())
(defvar *roles* '())
(defvar *projects* '())
(defvar *priorities* '())
(defvar *trackers* '())
(defvar *statuses* '())
(defvar *include-projects* '())
(defparameter *redmine-url* "http://localhost/redmine")
(defparameter *api-key* "824ebd412431d9f486f67a9af6d750df66dfca46")

(defun redmine-url (url-parts)
  (format nil "~{~a~^/~}.json"
          (cons *redmine-url*
                (mapcar #'princ-to-string (ensure-list url-parts)))))

(defun redmine-request (method url-parts &key limit offset sort impersonate content parameters)
  (multiple-value-bind
        (response status headers actual-uri stream should-close-p status-line)
      (apply #'drakma:http-request
             (redmine-url url-parts)
             :method method
             :additional-headers (append
                                  (when impersonate
                                    (list (cons "X-Redmine-Switch-User" impersonate)))
                                  (list (cons "X-Redmine-API-Key" *api-key*)))
             :external-format-in :utf-8
             :external-format-out :utf-8
             (if content
                 (list :content
                       (st-json:write-json-to-string content)
                       :content-type "application/json; charset=utf-8")
                 (list :parameters
                       (append
                        parameters
                        (when limit
                          (list (cons "limit" (princ-to-string limit))))
                        (when offset
                          (list (cons "offset" (princ-to-string offset))))
                        (when sort
                          (list (cons "sort" sort)))))))
    (declare (ignore headers actual-uri stream should-close-p))
    (case status
      ((200 201 304) nil)
      (404 (bot-error "object not found"))
      (t (bot-error "unexpected http status: ~a ~a" status status-line)))
    (case method
      ((:get :post)
       (simplify-json
        (st-json:read-json-from-string
         (ensure-string-not-octets
          response))))
      (t nil))))

(defun grab-all (url-parts key)
  (let ((offset 0)
        (total 0))
    (iter (let* ((result (redmine-request :get url-parts :limit 100 :offset offset))
                 (entries (getf result key)))
            (setf total (getf result :total-count))
            (incf offset (length entries))
            (appending entries into all))
          (while (< offset total))
          (finally (return all)))))

#++
(defun load-project (id)
  (redmine-request :get (list "projects" id)))

(defun list-projects ()
  (iter (for project in (grab-all "projects" :projects))
        (when (or (null *include-projects*)
                  (member (getf project :identifier) *include-projects* :test #'equal))
          (collect project))))

(defun list-users ()
  (grab-all "users" :users))

(defun list-roles ()
  (getf (redmine-request :get "roles") :roles))

(defun list-priorities ()
  ;; no need for GRAB-ALL because the list doesn't have limit/offset stuff
  (getf (redmine-request :get '("enumerations" "issue_priorities")) :issue-priorities))

(defun list-trackers ()
  ;; no need for GRAB-ALL because the list doesn't have limit/offset stuff
  (getf (redmine-request :get "trackers") :trackers))

(defun list-statuses ()
  (getf (redmine-request :get "issue_statuses") :issue-statuses))

(defun recent-issues (&key impersonate (limit 25) user)
  (getf (redmine-request
         :get '("issues")
         :parameters
         (list '("sort" . "updated_on:desc")
               (cons "assigned_to_id"
                     (if user
                         (princ-to-string
                          (object-id (find-by-substring '*users* '(:login :mail) user)))
                         "me")))
         :impersonate impersonate
         :limit limit)
        :issues))

(defun normalize-name (name)
  (string-downcase (cl-ppcre:regex-replace-all "\\s+" (trim name) " ")))

(defun find-by-substring (where fields text &optional (desc where) noerror-p)
  (let ((list (if (symbolp where) (symbol-value where) where)))
    (setf text (normalize-name text)
          fields (ensure-list fields))
    (flet ((plist-matches-p (plist)
             (iter (for field in fields)
                   (thereis (when (search text (normalize-name (getf plist field))) t)))))
      (or (find-if #'plist-matches-p list)
          (if noerror-p
              nil
              (bot-error "~a: search failed: '~a'" desc text))))))

(defun find-by-id (list value)
  (or (iter (for plist in list)
            (finding plist such-that (equal value (getf plist :id))))
      (bot-error "search failed: id ~a" value)))

(defun project (thing)
  (etypecase thing
    (cons thing)
    (number (find-by-id *projects* thing))
    (string (find-by-substring '*projects* '(:name :identifier) thing))))

(defun object-id (thing)
  (typecase thing
    (cons (getf thing :id))
    (string
     (rx-match-case thing
       ("^\\s*#?(\\d{1,100})\\s*$" (num) (values (parse-integer num)))
       (t (bot-error "bad object id ~s" thing))))
    (number thing)
    (t (bot-error "bad object id ~s" thing))))

(defun load-redmine-data ()
  (setf *users*
        (list-users)
        *roles*
        (list-roles)
        *priorities*
        (list-priorities)
        *trackers*
        (list-trackers)
        *statuses*
        (list-statuses)
        *projects*
        (let ((watcher-role
                (or (find-by-substring *roles* :name "watcher" 'roles t)
                    (find-by-substring *roles* :name "наблюдатель" 'roles t))))
          (when watcher-role (setf watcher-role (object-id watcher-role)))
          (iter (for project in (list-projects))
                (handler-case
                    (labels ((unproject (plist)
                               (remove-from-plist plist :project))
                             (grab (name)
                               (mapcar #'unproject
                                       (grab-all
                                        (list "projects" (getf project :id) (snakify name))
                                        name))))
                      (let ((memberships (grab :memberships)))
                        (setf (getf project :members)
                              (iter (for membership in memberships)
                                    (collect (find-by-id *users* (getf (getf membership :user) :id))))
                              (getf project :categories)
                              (grab :issue-categories)
                              (getf project :versions)
                              (grab :versions)
                              (getf project :watchers)
                              (when watcher-role
                                (iter (for membership in memberships)
                                      (when (iter (for role in (getf membership :roles))
                                                  (thereis (= (getf role :id) watcher-role)))
                                        (collect (object-id (getf membership :user)))))))
                        (collect project)))
                  (bot-error ()
                    (warn "failed to load project: ~s ~a"
                          (getf project :identifier)
                          (getf project :name))))))))

(defun find-in-project (project project-field field text)
  (find-by-substring (getf (project project) project-field) field text
                     (format nil "project field ~a" project-field)))

(defun post-issue (project &key subject description assigned-to version category tracker priority impersonate)
  (setf project (project project))
  (when subject (setf subject (trim subject)))
  (unless (null-if-empty subject)
    (bot-error "must provide subject"))
  (when description (setf description (trim description)))
  (unless (null-if-empty description)
    (bot-error "must provide description"))
  (let ((result
          (redmine-request
           :post "issues"
           :impersonate impersonate
           :content
           (st-json:jso "issue"
                        (apply #'st-json:jso
                               "project_id" (object-id project)
                               "subject" subject
                               "description" description
                               (append
                                (when assigned-to
                                  (list
                                   "assigned_to_id"
                                   (object-id (find-in-project project :members '(:login :mail) assigned-to))))
                                (when version
                                  (list
                                   "fixed_version_id"
                                   (object-id (find-in-project project :versions :name version))))
                                (when category
                                  (list
                                   "category_id"
                                   (object-id (find-in-project project :categories :name category))))
                                (when tracker
                                  (list
                                   "tracker_id"
                                   (object-id (find-by-substring '*trackers* :name tracker))))
                                (when priority
                                  (list
                                   "priority_id"
                                   (object-id (find-by-substring '*priorities* :name priority))))
                                (when-let ((watchers (getf project :watchers)))
                                  (list "watcher_user_ids" watchers))))))))
    (getf result :issue)))

(defun update-issue (issue &key subject description status impersonate)
  (redmine-request
   :put (list "issues" (object-id issue))
   :impersonate impersonate
   :content
   (st-json:jso "issue"
                (apply #'st-json:jso
                       (append
                        (when subject
                          (list "subject" subject))
                        (when description
                          (list "description" description))
                        (when status
                          (list
                           "status_id"
                           (object-id (find-by-substring '*statuses* :name status)))))))))

(defun issue-info (issue)
  (getf (redmine-request
         :get (list "issues" (object-id issue)))
        :issue))

(defun issue-url (issue)
  (format nil "~a/issues/~a" *redmine-url* (object-id issue)))

;; TBD: use lazy loading for project info
;; TBD: handle 'errors' response
;; e.g. (:ERRORS ("Тема не может быть пустым"))
;; TBD: watchers
;; TBD: display issue url in the response

;; TBD: fail on ambiguous name searches

;; TBD: (later) issue search (non-api thing)
;; TBD: (later) issue status setting
;; TBD: (later) issue relations
;; TBD: (later) (в работе)
