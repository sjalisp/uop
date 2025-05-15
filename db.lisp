(in-package :cw-uop)
(defmethod update-by-id (collection id modifications)
  (coll-update collection '(:eq "_id" id) modifications))

;; database is initialized with database parameters and modified
;; by a multistenant strategy. It exposes persistent content via
;; collections.
;; a database is used by a db-interface which access database collections

(defclass database ()
   ((connection-params)
    (metadata :accessor metadata :initform (make-hash-table))
    (collections :reader collections :initform (make-hash-table))))

(defgeneric get-metadata (db)
  (:documentation "load if needed main db collections and make metadata available"))
(defgeneric get-collection (db name))


(defmethod get-collection ((db database) name)
  (gethash name (collections db)))

(def get-meta-collection ((db database) meta-description))

(def get-instance-collection ((db database) (cls metaclass))
  (let ((name (random-name db)))
    (get-collection db name)))

(defclass metainfo ()
  ((by-id :reader by-id :initform (make-hash-table))
   (by-name :reader by-name :initform (make-hash-table))
   (collection :reader collection)))


(defun map-subset (hashtable keys)
  (let ((submap (make-hash-table)))
    (dolist (key keys)
      (setf (gethash key submap) (gethash key hashtable)))))

(defmethod map-by-id ((meta metainfo) lst)
  (mapcar (lambda (id) (gethash id (by-id meta))) lst))

(defmethod open-db ((db t) &optional user-id default-data)
  (when (not (has-collecion db "metaclass"))
    (create-metadata db)))

(defun load-metatype (database collection-name)
  (let ((metatype (make-instance 'metainfo :collection (get-collection database collection-name))))
    (dolist (metaobject (find (collection metatype)))
      (setf (gethash (by-id metatype) (_id metatype)) metaobject)
      (setf (gethash (by-name metatype) (name metatype) metaobject)))))

(defun add-inserts (collection changes)
  (dolist (record changes)
    (coll-insert collection record)))

(defun add-modifications (collection changes)
  (maphash (lambda (k v) (coll-update collection '(:eq "_id" k) v) changes)))

(defun add-deletions (collection changes)
    (dolist (id changes)
      (coll-remove-id collection id)))

(defun oid-class (db oid)
  (get-collection db (class-id k)))

(defun add-crud-changes (collection changes)
  (add-inserts collection (inserted changes))
  (add-modifications collection (modifed changes))
  (add-deletions collection (deleted changes)))

(defun add-asociation-changes (collection changes)
  (add-inserts collection (inserted changes))
  (add-deletions collection (deleted changes)))

(defun process-object-changes (db changes)
  (maphash
   (lambda (k v)
     (coll-insert (oid-class db k) v))
   (inserted changes))

  (maphash
   (lambda (k v)
     (coll-update (oid-class db k) k v))
   (modified changes))

  (dolist (oid (deleted changes))
      (coll-remove-id (oid-class db oid) oid)))

(defmethod handle-commit ((db database) (changes ChangeSet))
  (add-crud-changes 
   (get-collection  db "metaclass") (classes changes))                 
  (add-crud-changes 
   (get-collection db "metattr") (attributes changes))                 
  (add-crud-changes 
   (get-collection db "metarole") (roles changes))                 
  (add-crud-changes 
   (get-collection db "metatag") (tags changes))                 
  (add-crud-changes 
   (get-collection db "metagroup") (groups changes))
  (add-crud-changes
   (get-collection db "metaclass") (classes changes))
  (process-object-changes db (objects changes))
  (add-association-changes (tagged changes))
  (add-association-changes (related changes))
  (add-association-changes (grouped changes))
  (commit db)
  (store-changes (db changes)))


(defmethod get-object ((db database) oid)
  (coll-get (get-collection db (class-id oid)) oid))

(defun related-object (related-record oid)
  (if (eql (gethash "subject" related-record) oid) "object" "subject"))

(defun related-role (related-record) (gethash "role" record))

(defun get-all-related (cursor oid)
  (let ((the-map (make-hash-table)))
    (labels
        ((get-role-related (role)
           (ensure-hash-key role the-map ()))
         (process-record (record)
           (letrec ((role (related-role record))
                 (role-list (get-role-related role)))
             (push (related-object record oid) role-list))))
      (mapcar process-record cursor))
    the-map))

(defun tagged (db)
  (get-collection db "tagged"))

(defun related (db)
  (get-collection db "related"))

(defun grouped (db)
  (get-collection db "grouped"))

(defun metaclass (db)
  (get-collection db "metaclass"))

(defun metarole (db)
  (get-collection db "metarole"))

(defun metatag (db)
  (get-collection db "metatag"))

(defun metagroup (db)
  (get-collection db "metagroup"))

(defun process-related (cursor oid)
  (labels ((other (record)
             (if (eql (gethash "subject" record) oid)
                 "object" "subject")))
    (mapcar other cursor)))

(defun foo (db oid &optional role)
  (let* (
           (subject-match '(:eq "subject" oid))
           (object-match '(:eq "subject" oid))
	   (oid-match '(:or subject-match object-matche))
           (and-role '(:and (:eq "role" role)))
	   (criteria (if role and-role oid-match))
	   (the-map (make-hash-table))
           )))

(defmethod get-related-to-object ((db database) oid &optional role)
  (let* (
           (subject-match '(:eq "subject" oid))
           (object-match '(:eq "subject" oid))
	   (oid-match (list :or subject-match object-matche))
           (and-role '(:and (:eq "role" role)))
	   (criteria (if role and-role oid-match))
	   (the-map (make-hash-table)))
    (labels ((other-object (record)
	       (if (eql oid (subject record)) (subject record) (object record))))
      (dolist (record (find (related db) criteria))
	(push (other-object record) (ensure-hash-key (role record) the-map '()) ))
      (if role (gethash role the-map)  the-map))))

(defmethod get-object-tags ((db database) &optonal oid)
  (let ((collection (tagged db))
        (the-map (make-hash-table))
	(criteria (if oid '("object" oid) nil))
        (cursor
         (if oid
             (find collection (:eq "object" oid))
             (find collection))))
    (dolist (record cursor)
      (push (ensure-hash-key (object record) the-map '()) (tag record)))
    (if oid
	(gethash oid the-map)
	(the-map))))

(defmethod get-tag-objects ((db database) &optonal tag)
  (let* ((collection (tagged db))
        (the-map (make-hash-table))
	(criteria (if tag '("tag" tag) nil))
        (cursor (find collection criteria)))
    (dolist (record cursor)
      (push (ensure-hash-key (tag record) the-map '()) (object record)))
    (if tag
	(gethash tag the-map)
	(the-map))))

(defun best-and-order (clauses)
  (clauses))


(defmethod eval-clause (db (the-clause AndClause))
  (let ((ordered-clauses (best-and-order (clauses the-clause)))
        (subresults ())
        (res nil))
    (setf res (eval-clause (car ordered-clauses)))
    (dolist (clause (cdr ordered-clauses))
      (setf res (intersect res (eval-clause clause)))
      (if (not res)
          (return res)))
    res))

(defmethod eval-clause (db (the-clause OrClause))
  (union (mapcar #'eval-clause (clauses the-clause))))

(defmethod eval-clause (db (the-clause PropertyClause)))

(defgeneric open-db (db &optional user-id default-data)
  (:documentation "initialeze database setting up its collections and optionally giving it starting metadata"))

(defgeneric load-metadata (db)
  (:documentation "loads the databases metadata into memory from its db specific collections"))

(defgeneric load-metaclass (db)
  (:documentation "loads classes and their attributes and superclasses"))


