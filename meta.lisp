(in-package :cw-uop)

(defun make-attribute-kv (name type &key (editable nil))
  '(:_id (index 48) :name name :type type :editable editable))

(defun make-class-kv (name supers attributes)
  '(:_id (index 64) :name name :supers supers :attributes attributes))

(defvar *known-classes* (make-hash-table :test #'equal))

(defclass metadb-object ()
  ((id :accessor id :initarg :id :initform (index 48))
   (name :accessor name :initarg :name)
   (collection :accessor collection)
   (_transients :reader transients :initform (make-hash-table))
   ))

(defgeneric metaobject<-db (metaobject data))
(defgeneric metaobject->db (metaobject))

(defmethod metaobject->db ((metaobject metadb-object))
  (upsert (collection metaobject) (persistent-data metaobject)))

(defmethod db ((metaobject metadb-object))
  (db (collection metaobject))
)

(defmethod get-transient ((object metadb-object)  key &key init-value nil-value)
  (let ((val (gethash key (transients object))))
    (if val val 
        (if init-value 
	    (setf (gethash key (transients object)) init-value)))))

(defclass meta-permissions ()
  ((sys-defined :accessor sys-defined :initarg :sys-defined :initform nil)
   (app-defined :accessor app-defined :initarg :app-defined :initform nil)
   (user-defined :accessor user-defined :initarg :user-defiend :initform t)
   (mutable :accessor mutable :initarg :mutable)
   (deletable :accessor deletable :initarg :deleteable)))

 
(defclass metaclass (metadb-object) 
  ((attr-ids :accessor attr-ids :initform '())
   (attributes)
   (name :initarg :name)
   (superclass :accessor superclass :initarg :superclass :initform "")
   (indices :reader indices :initform () :initarg :indices)
   (permisions :reader permissions :initform (make-instance 'meta-permissions))
   (user-defined :reader user-defined :initarg :user-defined)))

(defgeneric attributes (mcls))
(defgeneric supers (mcls))
(defmethod attributes ((mcls metaclass))
  (let ((attrs (attributes (db mcls))))
  (get-transient 
   mcls :attributes 
   (mapcar (lambda (a) (gethash x attrs)) (attrs cls)))))
 
;;first problem
(defmethod supers (cls metaclass)
  (let ((db-supers (superclasses (db cls))))
    (get-transient
     cls :superclasses
     (mapcar (lambda (s) (gethash s db-supers)) (supers cls)))))


(defgeneric get-attribute (class name))

(defmethod index-of-attribute ((class metaclass) (name String))
  (position name class :test #'(lambda (x y)   (equal (get-name y) x))))

(defclass named-type ()
  ((name :accessor name :initarg :name)
   (type :accessor type :initarg :type)))

(defclass meta-attribute(named-type metadb-object) 
  ((description :accessor description)
   (editable :reader editable :initarg :editable)))

(defmethod get-attribute ((class metaclass) (name String))
  (find name (attributes class) :test #'(lambda (x y) (equal (slot-value y 'name) x))))

(defun create-class (name &key attributes supers indices)
  (if (gethash name *known-classes*) nil 
    (setf (gethash name *known-classes*) (make-instance 'metaclass :name name :attributes attributes :supers supers :indices indices))))

(defmethod add-attribute ((class metaclass) name type) 
  (if (not (get-attribute class name)) 
    (setf (attributes class) (nconc  (attributes class) (list (make-instance 'named-type :name name :type type))))))

(defun get-loid-parts (loid)
  (let* ( (lstring (substitute #\  #\. loid)) 
         (vstring (concatenate 'string "(vector " lstring ")")))
    (read-from-string vstring)))

(defclass metarole (metadb-object)
  ((arity :accessor arity)
   (to-class :accessor to-class)
   (reverse-role :accessor reverse-role)))

