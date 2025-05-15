(defpackage :tt.uop.clsql-db
  (:use :tt.uop.db :clsql-user))

(in-package :tt.uop.clsql-db)

(defclass clsql-db (RDBMS)
  ((pool :reader pool :initform () )
   (pool-mutex :reader pool-mutex :initform (sb-thread:make-mutex :name "pool"))))

(defmethod get-connection ((db clsql-db))
  (sb-thread:with-mutex ((pool-mutex db))
    (let ((con (pop (pool db))))
      (if con con (new-connection db)))))

(defmethod release-connection ((db clsql-db) con)
  (push con (pool db)))

(defmethod setup-rels ((db clsql-db))
  (create-table [roles]
                '(([id] integer)
                  ([name] string)
                  ([rname1] string)
                  ([min1] integer)
                  (lmax1] integer)
                  ([type1] integer)
                  ([rname2] string)
                  ([min2] integer)
                  (lmax2] integer)
                  ([type2] integer)) :database db)
  (create-table [u_reldata]
                '(([id] integer)
                  ([loid1] string)
                  ([loid2] string))) :database db)

(defmethod setup-classes ((db clsql-db))
  (create-table [u_class]
                '(([id] integer :type :primary)
                  ([name] string)
                  ([instantiable] boolean)
                  ([table_name] string)
                  ([max_soq] integer)):database db)

  (create-table [u_supers]
                '(([subclass] integer)
                  ([superclass] integer)) :database db)

  (create-table [u_attribute]
                '(([class_id] integer :not-null)
                  ([index] integer :not-null)
                  ([name] string)
                  ([type] integer)
                  ([class] string)
                  ([db_name] string))) :database db)

(defmethod setup-types ((db clsql-db))
  (create-table [u_type]
                `(([id] integer)
                  ([name] string)
                  ([inner1] id)
                  ([inner2] id)
                  ([db-table] string))) :database db)

(
