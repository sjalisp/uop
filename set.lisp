(in-package :cw-uop)
(defclass set-abstraction () ())
(defgeneric add (a-set item))
(defgeneric set-remove (a-set item))
(defgeneric && (&rest sets))
(defgeneric intersect (set-1 set-2))
(defgeneric set-union (set-1 set-2))
(defgeneric || (&rest sets))
(defgeneric -- (set-1 set-2))
(defgeneric size (a-set))
(defgeneric contains (a-set item))
(defgeneric in-both (smaller larger))

(defclass hash-set (set-abstraction)
  ((contents :reader contents :initform (make-hash-table))))

(defmethod set-add ((a-set hash-set) &rest items)
  (dolist (item items)
    (setf (gethash item (contents a-set)) t)))

(defmethod set-remove ((a-set hash-set) val)
  (remhash val (contents a-set)))

(defmethod size ((a-set hash-set))
  (hash-table-count (contents a-set)))

(defmethod contains ((a-set hash-set) item)
  (multiple-value-bind (val found) (gethash item (contents a-set))
                    found))

(defmethod in-both ((smaller hash-set) larger)
  (let ((result (make-instance 'hash-set)))
    (dolist (item (elements smaller))
      (if (contains larger item)
          (add result item)))
    result))

      

(defmethod set-intersection ((set-1 hash-set) (set-2 hash-set))
  (let ((res (make-instance 'hash-set))
        (members (intersection (elements set-1) (elements set-2))))
    `(set-add res ,@members)))

(defmethod copy ((a-set hash-set))
  (let ((res (make-instance 'hash-set)))
    (set-add res (elements a-set))
    res))

(defmethod && (&rest sets)
  (let ((res (if (car sets) (copy (car sets)) nil))
        (remaining (cdr sets)))
    (if remaining
      (while res
          (dolist (set remaining)
            (setq res (intersection res set)))))
    res))

(defmethod elements ((a-set hash-set))
  (let ((res))
    (loop for key being the hash-keys of (contents a-set)
        do (push key res))
    (nreverse res)))

