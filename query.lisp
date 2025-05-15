(in-package :cw-uop)

(defclass QueryClause () ())

(defgeneric convert-to-db-query (db query-component))

(defclass CombinationClause (QueryClause) 
  ((clauses :reader clauses :initarg :clauses :initform '())))

(defclass AndClause (CombinationClause) ())

(defclass OrClause (CombinationClause) ())

(defclass PropertyClause (QueryClause)
  ((property-name :initarg :property-name :reader property-name)
   (operator :initarg :operator :reader operator)
   (value :initarg :value :reader value)))



(defclass ClassClause (QueryClause)
  (
   (class-ids :accessor class-ids :initarg :class-ids :initform '())
   (quantifier :accessor quantifier :initform :quantifier :initarg :all)))

(defclass TagClause (QueryClause)
  (
   (tag-ids :accessor tag-ids :initarg :tag-ids )))

(defclass AnyClause ()
  (
   (query-runner :accessor query-runner)
   (ids :accessor ids)))

(defgeneric negated-clause (clause))

(defmethod negated-clause ((clause PropertyClause))
  (let ((new-operator
	 (case (operator clause)
	   (:gt :le)
	   (:lt :ge)
	   (:le :gt)
	   (:ge :lt)
	   (:eq :ne)
	   (:ne :eq))))
    (make-instance 'PropertyClause
		   :property-name (property-name clause)
		   :operator new-operator
		   :value (value clause))))

(defgeneric simply-query (clause &optional db))
(defun read-external-query (stream))
(defgeneric externalize-query (clause))

