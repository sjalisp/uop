(defun assoc->dict (kvs)
  (let ((table (make-hash-table)))
    (dolist (p (group 2 kvs))
      (setf (gethash (car p) table) (cadr p)))
    table))

(defun assoc<-dict (hashmap)
)

(defmethod collection-insert ((collection mongo-collection) data-dict) 
  
