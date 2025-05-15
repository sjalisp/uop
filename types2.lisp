(in-package :tt.uop.meta)

;corresponding type definitions
;basic types for sanity checking input.  Should not be needed internally

(deftype int1 () `(integer ,(* -1 (expt 2 7)) ,(expt 2 7)))
(deftype int2 () `(integer ,(* -1 (expt 2 15)) ,(expt 2 15)))
(deftype int4 () `(integer ,(* -1 (expt 2 31)) ,(expt 2 31)))
(deftype int8 () `(integer ,(* -1 (expt 2 63)) ,(expt 2 63)))
(deftype uint1 () `(integer ,(expt 2 8)))
(deftype uint2 () `(integer ,(expt 2 16)))
(deftype uint4 () `(integer ,(expt 2 32)))
(deftype uint8 () `(integer ,(expt 2 64)))
(deftype float4() `single)
(deftype float8() `double)
(deftype object() `(satisfies legal-loid?))






(defclass gop-type () 
  ((symbol-of :reader symbol-of :initarg :symbol-of)
   (of-type :reader of-type :initarg :of-type)
   ))

;skip rigorous validation tests for most types for now TBD
;validation is only used checking user insert and mods before 
;attempting to write them to the database.  Assume for now all
;our clients play nice.

;but here is the general method
(defmethod validate-data ((type gop-type) data) 
  (typep data (of-type gop-type)))

;implementation in cache for types

(defun create-type (type-symbol &optional (of-type  t))
  (make-instance 'gop-type :symbol-of type-symbol :validator test))

(defclass general-object-type (gop-type) ()
  ((of-type :initform :object)))

(defclass object-type (general-object) 
  ((symbol-of :initform :object)))

(defclass reference-type (general-object) 
  ((symbol-of :initform :reference)))(deftype 

(defclass collection-type (gop-type)
  ((contentType :initArg contentType :initform :reference)))
  
(defclass set-type (collection-type)
   ((symbol-of :initform :set)))

(defclass list-type (collection-type)
  ((symbol-of :initform :list)))

(defclass vector-type (collection-type)
  ((symbol-of :initform :vector)))

(defclass map-type (gop-type)
  ((keyType :reader key-type :initArg :keyType :initform :reference)
   (valueType :reader value-type :initArg :valueType :initform :reference)
   (symbol-of :initform :map)))

(defvar *known-types* ())

(defun intialize-types () 
  (unless *known-types*
    (setf  *known-types* (make-hash-table :test 'equal))
    (create-type :char :of-type 'characterp)
    (create-type :byte :test 'bytep)
    (create-type :short :test 'shortp)
    (create-type :int :test 'integerp)
    (create-type :long :test 'integerp)
    (setf (gethash :char *known-types*)
          (make-instance 'simple-type :test 'characterp :symbol-of: :char))



(defmethod initialize-instance :after ((mgr type-manager) &rest args &key)
  (labels 
      ((make-class (symbol-of)
         (let* ((new_name (concatenate 'string (symbol-name symbol-of) "-TYPE"))
                (ptr (intern new_name))
                (junk (gensym)))
           (set ptr (eval `(defclass ,ptr (gop-type) ((symbol-of :initform ,symbol-of)))))))
       (make-types (symbols)           
           (dolist (nm symbols)
             (let ((cls (make-class nm)))
               (setf (gethash nm (known mgr)) (make-instance cls))))))
    
    (make-types '(:char :byte :short :int :long :float :double :string :queue :blob :boolean))))
  


(defmethod get-type ((mgr type-manager) style)
  (let ((found (gethash style (known mgr))))
    (if found found
        (case (car style)
          (:set 
           (let ((type (make-instance 'setType :contentType (get-type (cadr style)))))
             (setf (gethash style (known mgr)) type)
             type))
           (:map 
            (let* ((keyType (first (cdr style)))
                  (valuetype (second (cdr style)))
                  (type (make-instance 'setType 
                                       :keyType keyType
                                       :valueType valueType)))
              (setf (gethash style (known mgr)) type)
              type))
           (:vector 
            (let ((type (make-instance 'setType :contentType (get-type (cadr style)))))
             (setf (gethash style (known mgr)) type)
             type))
           (:struct
            (let* ((name (cadr style))
                  (subtypes (cddr style))
                  (type (make-instance 'structType :name name :subtypes subtypes)))
             (setf (gethash style (known mgr)) type)
             type))))))


