(defclass relation () 
  ((name :reader name :initarg :name)
   (id :reader id :initarg :id)
   (roles :reader roles :initarg :roles)))

(defclass relation-role () 
  ((name :reader name :initarg :name)
   (arity :reader arity :initarg :arity)
   (contents :reader contents :initform (make-hash-table))))

(defmethod get-contents ((role relation-role) of-loid)
  ; protect me
  (let ((related (gethash (contents role) of-loid)))
    (if (not related) 
)

