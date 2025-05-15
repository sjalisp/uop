(in-package :cw-uop)
;;wraps a collection/table of a db and tracks what db it is from
;;
(defclass attr-spec ()
  ((type :reader type :initarg :type)
   (name :reader name :initarg :name)
   (default :accessor default :initarg :default :initform nil)))


(defclass db-collecition ()
  ((name :accessor name :initarg :name)
   (db :accessor db :initarg :db)))

(defclass meta-db-collection (db-collection))

(defclass meta-tenant-collection (meta-db-collection)
  ((tenant-id :accessor tenant-id :initarg :tenant-id :initform nil)))

(defun search-criteria (attribute condition value)
  '(attribute contidition value))

(defclass TenantPolity ())

(defclass SingleUser (TenantPolicy))

(defclass EmbeddedTenancy (TenantPolicy)
  (
   (tenant-id :reader tenant-id :initarg :tenant-id)))

(defgeneric expand-condition (tenant-policy condition &keys collection))

(defvar *meta-collections*
  '(:classes "uop-classes" :attributes "uop-attributes" :roles "uop-roles"
	     :tags "uop-tags" :groups "uop-groups" :queries "uop-queries"
	     :users "uop-users" :schemas "uop-schemas" :applications
	     "uop-applications" :users "uop-users" :schemas "uop-schemas"))


(defgeneric tenant-search-criteria (ternant-policy))

(defmethod tenant-search-criteria ((tenancy  TenantPolicyy))
  nil)

(defmethod tenant-collection-name ((tenancy SingleUser) name)
  name)

(defmethod tenant-collection-name ((tenancy EmbeddedTenancy) name)
  
(defclass DatabaseCollections ()
  (
   (tenant-policy  :reader tenant-policy :initarg :tenant-policy :accessor tennant-policy)
   (database :reader database)
   (known-collections :reader known-collections :initform '())))

(defgeneric get-collection-named (collections name))
(defgeneric make-collection (collections name))

(defmethod make-collection ((collections DatabaseCollections) name)
  (
  

(defmethod get-collection-named ((collections DatabaseCollections) name)
  (let
      ((known (assoc collections name)))
    (if known
	known
      (setf (assoc collections name) (make-collection collections name)))))





 
    


(defclass DatabaseCollection ()
  ((database :initarg :database)
   (name :initarg :name)))

(defgeneric coll-get (collection oid))
(defgeneric coll-raw-get (collection criteria &optional only_columns))
(defgeneric coll-get-all (collection &optional only_columns))
(defgeneric coll-update (collection selector modifications))
(defgeneric coll-insert (collection data))
(defgeneric coll-delete (collection criteria))
(defgeneric coll-remove-id (collection id))
(defgeneric coll-find (collection criteria))
(defgeneric coll-find-one (collection criteria))
(defgeneric coll-upsert (collection data))

(defmethod coll-remove-id (collection id)
  (coll-delete collection '("_id" id)))

(defmethod coll-get (collection an-id)
  (coll-find-one collection '("id" an-id)))

(defmethod coll-find-ids (collection criteria)
  (coll-find (collection criteria '("id"))))

  
(defmethod coll-get-all (collection &optional only_columns)
  (coll-find collection nil only_columns))

(defmethod coll-raw-get (collection criteria &optional only_columns)
  (coll-find collection criteria only_columns))


   
