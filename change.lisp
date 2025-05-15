(in-package :cw-uop)

(defclass change-list () 
  ((new :accessor new :initform ())
   (changed :accessor changed :initform (make-hash-table))
   (deleted :accessor deleted :initform ())
   (rel-changes :reader rel-changes :initform (make-hash-table))
   (prop-changes :reader prop-changes :initform (make-hash-table))
   (set-like-adds :reader set-add :initform ())
   (set-like-removes :reader set-remove :initform ())
   (vec-like-adds :reader vec-add :initform ())
   (vec-like-removes :reader vec-remove :initform ())
   (map-adds :reader map-add :initform ())
   (map-removes :reader map-remove :initform())
   ))

(defmethod delete-object ((changes change-list) loid)
  (remove loid (new changes))
  (remhash loid (changed changes))
  (remhash loid (rel-changes changes))
  (remhash loid (prop-changes changes))
  (push loid (deleted changes))
)

(defclass modification-change ()
  ((object-ref :accessor ref :initarg :ref)
   (modified-fields :accessor fields :initform (make-set))))

(defgeneric get-object-graph (objects))


(defun prepare-changes (changes)
  "Remove references to deleted objects.  Persist reachable unpersistent objects"
  (labels 
      ((get-unpersisted-objects (object-refs)
         (dolist (obj object-refs)
           (filter 
            (lambda (object) (not persitent? object))
            (get-object-graph obj)))))
    
      (map #'make-persistent (get-unpersisted-objects (new changes)))
      (map #'make-persistent (get-unpersisted-objects (modified changes)))
      (remove-deleted (deleted changes) (new changes))
      (remove-deleted (deleted changes) (modified changes))))

        
