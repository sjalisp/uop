(in-package :cw-uop)

(defclass assoc-dict ()
  ((data :accessor data :initarg :data :initform '())))

(defgeneric dict-keys (adict))
(defgeneric dict-get (adict key))
(defgeneric dict-put (adict key val))
(defgeneric dict-values (adict))

(defmethod dict-keys ((adict assoc-dict))
  (mapcar (lambda (pair) (car pair)) (data adict)))
