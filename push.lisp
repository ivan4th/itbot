(in-package :itbot)

(defvar *pushbullet-api-key*)
(defvar *pushbullet-contact-cache* (make-hash-table :test #'equal))

(defun pushbullet-request (method place &optional parameters)
  (simplify-json
   (st-json:read-json-from-string
    (ensure-string-not-octets
     (drakma:http-request (concat "https://api.pushbullet.com/v2/" place)
                          :method method
                          :basic-authorization (list *pushbullet-api-key* "")
                          :parameters parameters
                          :external-format-in :utf-8
                          :external-format-out :utf-8)))))

(defun pushbullet-list-contacts ()
  (getf (pushbullet-request :get "contacts") :contacts))

(defun pushbullet-find-contact (name)
  (values
   (ensure-gethash
    name *pushbullet-contact-cache*
    (iter (for contact in (pushbullet-list-contacts))
          (when (or (string-equal name (getf contact :name))
                    (string-equal name (getf contact :email-normalized)))
            (return (getf contact :email-normalized)))
          (finally (bot-error "can't find pushbullet contact: ~s" name))))))

(defun pushbullet-send-notification (to title text)
  (pushbullet-request
   :post "pushes"
   (list (cons "type" "note")
         (cons "email" (pushbullet-find-contact to))
         (cons "title" title)
         (cons "body" text))))
