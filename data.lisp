(in-package :tt.uop.data)

(defclass object-info ()
  ((meta :reader meta :initarg :meta)
   (count :reader get-count :initform 0)
  (loid :reader loid :initarg :loid)))

(defmethod initialize-instance :after ((info object-info) &rest args &key)
  (labels ((init-from-class ()
             (setf (slot-value info 'loid) (make-new-loid (slot-value info 'class))))
           (init-from-loid ()
             (setf (meta info) (get-class (loid info)))))
    (if meta (init-from-class) (init-from-loid))))


(defclass uop-data ()
  ((info :reader info :initarg :info)
   (loaded :accessor loaded :initform nil)
   (values :accessor data-values :initarg :values  :initform ())))

(defmethod initialize-instance :after ((d uop-data) &rest args &key)
  (setf (slot-value d 'values) (make-array (length (attributes (meta (info d)))) :initial-element nil)))

;;;while owned attributes are a general concept they are mainly used with 
;;;more sophisticated types such as blob and collection typed attributes"
(defclass owned-attribute () 
  ((owner :accessor owner :initarg :owner)
   (name :reader name :initarg :name)
   (pos :reader pos )))

(defmethod initialize-instance :after ((attr owned-attribute) &rest args &key)
  (setf (slot-value attr 'pos) (get-position (owner attr) (name attr))))

(defmethod get-values ((d uop-data))
  (if (not (and (loaded d) (valid d)))
      (load-from-db d))
  (data-values d))
      
(defun get-value (owned-attr)
  (aref (get-values (owner owned-attr)) (pos owned-attr)))

(defun (setf get-value) (val owned-attr )
  (setf (aref (get-values (owner owned-attr)) (pos attr)) val))

(defmethod get-class ((d uop-data))
  (class (info d)))

(defmethod get-loid ((d uop-data))
  (loid (info d)))

(defmethod load-from-db ((d uop-data))
  (db-load-into (loid-database (get-loid d)) d)
  (setf (loaded d) t))


(defmethod get-attribute ((d uop-data) name)
  (make-instance 'owned-attribute :owner d :name name))

(defmethod get-position ((d uop-data) name)
  (get-attr-position (class (info d)) name))

(defmethod get-positional-value ((d uop-data) pos)
  (elt (values d) pos))

(defmethod (setf get-positional-value) (val (d uop-data) pos)
  (setf (elt (get-values d) pos) val))



  
