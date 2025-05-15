(defpackage :com.conceptwareinc.uop
  (:use :cl)
  (:nicknames :cw-uop)
  (:import-from :split-sequence :split-sequence)
  (:export
   :update-dict
   :put-dict
   :ChangeSet
   :CrudChange
   :AssociationChanges
   :combine-changes
   :record-insert
   :record-modification
   :record-deleted
   :DatabaseCollection
   :coll-get
   :coll-raw-get
   :coll-get-all
   :coll-update
   :coll-insert
   :coll-delete
   :coll-remove-id
   :coll-find
   :coll-find-one
   :QueryClause
   :PropertyClause
   :NotClause
   :CombinationClause
   :OrClause
   :AndClause
   :RelatedClause
   :TaggedClause
   :GroupedClause
   :simplify-query
   :eval-external-query
   :externalize-query
   :database
   :get-user-database
   :execute-query
   :metadata
   :apply-changes
   :changes-since
   :get-object
   :object-tags
   :tag-objects
   :related-objects
   :objects-in-group
   :groups-containing-object))

;; (load "db-collection.lisp")
;; (load "changeset.lisp")
;; (load "query.lisp")
;; (load "db.lisp")


;; (defpackage :tt.uop.meta
;;   (:use :cl)
;;   (:export :metaclass :get-attribute :create-class :add-attribute
;;            :named-type :get-loid-parts))

;; (defpackage :tt.uop.data
;;   (:use :cl :tt.uop.meta)
;;   (:export :object-info :uop-data :owned-attribute :get-value :get-class 
;;            :get-loid :get-values "(setf get-value)" :get-attribute 
;;            :get-position))

;; (defpackage :tt.uop.db-basic
;;   (:use :cl-user :tt.uop.meta :tt.uop.data)
;;   (:export :database :contains :load :open-db :run-query
;;            :related-to-by-role :related-to :satisfies-role))

;; (defpackage :tt.uop.db-internal
;;   (:use :cl-user :tt.uop.meta :tt.uop.data :tt.uop.db-basic)
;;   (:export :get-raw-contents :handle-commit :load :handle-rollback 
;;            :setup-rels :setup-classes :setup-types
;;            :load-rels :load-classes :load-types
;;            :exec-db-query :exec-db-update ))
