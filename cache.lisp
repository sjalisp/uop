(defclass server () 
  ((loid-map :reader loid-map :initform (make-hash-table))
   (txn-map :reader txn-map :initform (make-hash-table)) ;unsure
   (lock-mgr :reader lock-mgr :initform (make-instance LockManager))
   (validation :reader validation :initform (make-hash-table))
 ))

(defmethod output-representation((atype gop-type) data)
  data
)

(defmethod input-representation((atype gop-type) data) 
  data
)

(defmethod input-representation ((atype set-type) data) 
  (let ((rep make-hash-table))
    (dolist (element data) 
      (setf (gethash element rep) :included))
    rep)
)

" change set operations
(insert-obj id data)
(delete-obj id-list)
(modify-obj id field-mod-list)
(relate relid pairs)
(unrelate relid pairs)
(unrelate-object relid half-pairs)
(unrelate-all id)
(drop-relation relid)
"

(defmethod get-object ((s server) loid)
  ;thread protect
  (multiple-value-bind (val present) (gethash loid (loid-map s))
    (if (not present) 
        (setf (gethash (loid-map s)) (make-instance 'data :loid loid))))
  (gethash (loid-map s) loid))

(defmethod valid ((s server) key)
  (not (gethash key (validation s))))

(defmethod (setf valid) (bool (s server) key)
  (let ((count (gethash key (validation s))))
    (if bool 
        (setf count (if count (1+ count) 1))
        (setf count (if (and count (> count 0)) (1- count))))
    (if (= count 0) (remhash key (validation s)) 
        (seth (gethash key (validation s)) count))))


