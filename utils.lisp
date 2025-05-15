(in-package :com.conceptwareinc.uop)

(defun update-dict (a-dictionary other-dictionary)
  (maphash
   (lambda (k v) (setf (gethash k a-dictionary) v))
   other-dictionary))

(defun put-dict (a-dict key value)
  (setf (gethash a-dict key) value))

(defun make-oid (cls)
  (format nil "~a.~a" cls (index36 64)))

(defun index36 (bits)
  (write-to-string (random (ash 1 bits)) :base 36))

(defun is-same-class (oid1 oid2)
  (eql (class-id oid1) (class-id oid2)))

(defun class-id (oid)
  (cadr (split-sequence #\. oid)))



  
