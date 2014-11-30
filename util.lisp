(in-package :itbot)

(defun dasherize (s)
  (make-keyword (substitute #\- #\_ (string-upcase s))))

(defun snakify (s)
  (substitute #\_ #\- (string-downcase s)))

(defun ensure-string-not-octets (thing)
  (if (stringp thing)
      thing
      (babel:octets-to-string thing :encoding :utf-8)))

(defun simplify-json (json)
  (typecase json
    (st-json:jso
     (let ((result '()))
       (st-json:mapjso
        #'(lambda (k v)
            (push (dasherize k) result)
            (push (simplify-json v) result))
        json)
       (nreverse result)))
    (proper-list
     (mapcar #'simplify-json json))
    (t json)))
