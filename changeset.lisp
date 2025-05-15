(in-package :cw-uop)


(defclass ChangeCollection ()
  ((inserted :accessor inserted :initform (make-hash-table))
   (deleted :accessor deleted :initform nil)))

(defgeneric insert-change (change-collection item)

(defclass CrudChange (ChangeCollection)
  ((modified :accessor modified :initform (make-hash-table))))

(defclass ObjectChanges (CrudChange) ())
(defclass ClassChanges (CrudChange) ())
(defclass TagChanges (CrudChange) ())
(defclass RoleChanges (CrudChange) ())
(defclass GroupChanges (CrudChange) ())
(defclass AttributeChanges (CrudChange) ())

(defclass AssociatehChanges (ChangeCollection)
  ((inserted :initarg inserted :initform '())
  (deleted :initarg deleted :initform '())))

                               t                 
(defclass TaggedChanges (AssociatedChanges)())
(defclass GroupedChanges (AssciatedChanges)())
(defclass RelatedChanges (AssociatedChanges)())

(defclass HasAttributeChanges () 
  ((attribute :initarg :attribute :accessor attribute)
   (class :initarg :class :accessor attr-class)))

(defmethod object-matches ((assoc-change associated) oid)
  (eql oid (object-id assoc-change))

(defmethod clssa-matches ((assoc-change associated) clsid)
  (eql clsid (class-id (object-id assoc-change))))

(defmethod object-matches ((assoc-change related-record) oid)
  (or
   (eql oid (subject-id associated))
   (eql oid (object-id associated))))

(defmethod objects ((record associated))
  (list (object record)))

(defmethod objects ((record related-record))
  (list (object-id record) (subject-id record)))

(defclass ChangeSet () 
  ((objects :accessor objects :initarg objects :initform (make-instance 'ObjectChanges))
   (classes :accessor classes :initarg classes :initform (make-instance 'ClassChanges))
   (attributes :accessor attributes :initarg attributes :initform (make-instance 'AttributeChanges))
   (roles :accessor roles :initarg roles :initform (make-instance 'RoleChanges))
   (tags :accessor tags :initarg :tags :initform (make-instance 'TagChanges))
   (groups :accessor groups :initarg groups :initform (make-instance 'GroupChanges))
   (grouped :accessor grouped :initarg grouped :initform (make-instance 'GroupedChanges))
   (related :accessor related :initarg related :initform (make-instance 'RelatedChanges))
   (tagged :accessor tagged :initarg tagged :initform (make-instance 'TaggedChanges))))


(defgeneric add-changes (changeset other-changeset))


(defgeneric add-modification (change-collection oid modifications))
(defgeneric add-insert (change-collection inserted))
(defgeneric add-delete (change-collection deleted changeset))
(defgeneric handle-object-delete (change-collection deleted))
(defgeneric handle-class-delete (change-collection deleted))
(defgeneric handle-tag-delete (changeset change-collection deleted))
(defgeneric combine-changes (existing new-changes))

(defmethod combine-changes ((existing CrudChange) (new-changes CrudChange))
  (dolist (insert (inserted new-changes))
    (add-insert existing insert))
  (maphash 
   (lambda (oid modification)
     (add-modification existing modify))
   (modified new-changes))
  (dolist (delete (deleted new-changes))
    (add-delete deletes delete)))

(defmethod combine-changes
    ((existing AssociationChanges)(new-changes AssociationChanges))
  (dolist (insert (inserted new-changes))
    (add-insert existing insert))
  (dolist (deleted (deleted new-changes))
    (add-delete existing  deleted)))

(defmethod add-modification ((change-collection CrudChange)  oid modifications)
  (let ((insert (gethash oid (inserted change-collection))))
    (if insert
	(update-dict insert modifications)
	(setf (gethash oid (modified change-collection)) modifications))))

(defun inserted-oid (inserted) 
  (gethash "id" inserted))

(defmethod add-insert ((change-collection CrudChange) inserted)
 (setf (gethash (inserted-oid inserted) (inserted change-collection)) inserted))
(defmethod add-delete ((change-collection CrudChange) deleted changeset)
  (let ((was-inserted (gethash (inserted change-collection) deleted))
	(was-modified (gethash (modified change-collection) deleted)))
    (cond
      (was-inserted (remhash deleted (inserted change-collection)))
      (was-modified (remhash deleted (modified change-collection)))
      (t (push deleted (deleted change-collection))
	 (handle-deletion change-collection changeset deleted)))))

(defmethod handle-delete ((change-collection ObjectChanges) changeset deleted)
  (handle-object-delete (related changeset) deleted)
  (handle-object-delete (tagged changeset) deleted)
  (handle-object-delete (grouped changeset) deleted))

(defmethod handle-class-delete ((changes HasAttributeChanges) deleted)
  (labels ((test (x) (eql (attr-class x) deleted)))
    (set-without test (inserted changes))
    (set-without test (deleted changes))))
          
(defmethod handle-changeset-delete ((change-collection ClassChanges) changeset deleted)
  (handle-class-delete (objects changeset) deleted)
  (handle-class-delete (related changeset) deleted)
  (handle-class-delete (grouped changeset) deleted)
  (handle-class-delete (has-attribute changeset) deleted)
  (handle-class-delete (tagged changeset) deleted))

(defun remhash-class-equal-oid (hashmap oid)
  (dolist
      (key
	(remove-if-not
	 (lambda (x) (is-same-class x oid))
	 (maphash-keys (lambda (x) x) hashmap)))
    (remhash key hashmap)))

(defmacro transform-place (transform place)
  `(setf ,place (funcall ,transform ,place)))

(defmacro set-without (test place)
  `(setf ,place (remove-if #',test ,place)))

(defmethod handle-class-delete ((changes ObjectChanges) deleted)
  (remhash-class-equal-oid (inserted changes) deleted)
  (remhash-class-equal-oid (modified changes) deleted)
  (set-without (lambda (x) (eql x deleted)) (deleted changes)))

(defun handle-role-delete (changes role)
  (labels
      ((test (x) (eql role (role x))))
    (set-without test (inserted changes))
    (set-without test (deleted changes))))

(defun entry-matches (match-fn)
  (lambda (assoc-change)
    (some #'match-fn (objects assoc-change))))

(defun oid-test (oid)
  (lambda (x) (eql x oid)))

(defun class-test (cls)
  (lambda (x) (eql (class-id x) cls)))

(defun has-object (oid) 
  (entry-matches (lambda (x) (eql x oid))))

(defun has-class-instance (cls-id)
  (entry-matches (lambda (x) (eql (class-id x) cls-id))))

(defmethod handle-object-delete ((changes AssociationChanges) deleted)
  (prune-changes changes (has-object deleted)))

(defmethod handle-class-delete ((changes AssociationChanges) deleted)
  (prune-changes changes (has-class-instance  deleted)))

(defun delete-if-key (key-test hashmap)
  (maphash 
   (lambda (key value) 
     (if (funcall key-test key) (remhash key hashmap)))
   hashmap))

(defun without-keys-satisfying (test hasmap)
  (let ((res (make-hash-table)))
    (maphash
     (lambda (k v)
       (if (not (funcall test k)) 
           (setf (gethash k res) v)))
     hashmap)
    res))

(defmethod prune-changes ((changes CrudChange)  object-test)
  (without-keys-satisfying  #'object-test (inserted crud-changes))
  (delete-if-key #'object-test (modified crud-changes))
  (delete-if-key #'object-test (deleted crud-changes)))

(defmethod  prune-changes ((changes AssociationChanges) object-test)
  (setf
   (inserted changes)
   (remove-if #'object-test (inserted changes)))
  (setf
   (deleted changes)
   (remove-if #'object-test (deleted changes))))
  ;(set-without object-test (inserted changes))
  ;(set-without object-test (deleted changes))

(defmethod handle-deletion ((change-collection ClassChanges) changeset cls-id)

  (delete-if-key (has-class-instance cls-id) (objects changeset))
  (handle-class-delete (related changeset) cls-id)
  (handle-class-delete (tagged changeset) cls-id)
  (handle-class-delete (has-attribute changeset) cls-id)
  (handle-class-delete (grouped changeset) cls-id))

(defmethod handle-deletion ((change-collection RoleChanges) changeset role)
  (prune-changes 
   (related changeset)
   (lambda (x) (eql (associated-role x) role))))

(defmethod handle-deletion ((change-collection TagChanges) changeset tag)
  (prune-changes
   (tagged changeset)
   (lambda (x) (eql (tag x) tag)))) 

(defmethod handle-deletion ((change-collection GroupChanges) changeset group)
  (prune-changes
   (tagged changeset)
   (lambda (x) (eql (associated-group x) group))))

(defun record-insert (changeset data type)
  (let ((collection
	 (case type
	   (:object (objects changeset))
	   (:role (roles changeset))
	   (:group (groups changeset))
	   (:tag (tags changeset))
	   (:attribute (attributes changeset))
	   (:related (related changeset))
	   (:grouped (grouped changeset))
	   (:tagged (tagged changeset)))))
    (add-insert collection data)))

(defun record-modification (changeset key data type)
  (let ((collection
	 (case type
	   (:object (objects changeset))
	   (:role (roles changeset))
	   (:group (groups changeset))
	   (:tag (tags changeset))
	   (:attribute (attributes changeset)))))
    
         (add-modification collection data)))
 
(defun record-delete (changeset data type)
  (let ((collection
	 (case type
	   (:object (objects changeset))
	   (:role (roles changeset))
	   (:group (groups changeset))
	   (:tag (tags changeset))
	   (:attribute (attributes changeset))
	   (:related (related changeset))
	   (:grouped (grouped changeset))
	   (:tagged (tagged changeset)))))
    (add-delete collection data)))

	


