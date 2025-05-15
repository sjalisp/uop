(defun convert_from_mezzanine (db)
  (fill-from-portal-db db (create-blank-db))

(defclass database ()
    ((name :accessor name)
     (credentials)
     (init-function :accessor init-function :initarg :init :initform nil)))

  
  (defgeneric open-db ((db database))
    )
  (defgeneric db-collections ((db database))
  )
(defgeneric load-metadata ((db database)))
(defgeneric load-classes ((db database)))
(defgeneric load-relations ((db database)))
(defgeneric load-tags ((db database)))
(defgeneric load-groups (db database)))
(defgeneric class-extent ((db database)))
(defgeneric get-object ((db database) oid))
(defgeneric get-related ((db database) oid))
(defgeneric related ((db database) :optional object role))

(defmethod related((db database) &optional object role)
  (when (or object role)
    (cond
      ((not object) ))

