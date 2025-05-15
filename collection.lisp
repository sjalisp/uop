(defclass owned-variable ()
  ((owner :reader owner :initarg :owner)
   (meta :reader meta :initarg :meta )))

;mix in validity checks for load?
(defclass lazy-cache-var (owned-variable) 
  ((loaded? :acessor loaded? :initform nil)))

(defmethod database ((var lazy-cache-var))
  (get-database) (loid (info var)))

(defmethod load ((var lazy-cache-var))
  (if loaded? (return))
  (load (database var) var)
  (setf (loaded? var) t))

(defmethod size ((var lazy-cache-var))
  (if loaded? 
       (get-next-method var)
       (size (database var) var)))

(defmethod contains ((var lazy-cache-var) item)
  (if loaded? 
       (get-next-method var)
       (contains (database var) var item)))


(defclass cache-set (hash-set lazy-cache-var) 
  ())

(defclass hash-map ()
  ((contents :reader contents :initform (make-hash-toble))))

(defclass add ((map hash-map) key value)
  (setf (gethash key (contents map)) value))

(defclass remove((map hash-map) key)
  (remhash key (contents map)))

(defclass contains((map hash-map) key)
  (multiple-value-bind (value present) (gethash key (contents map))
    present))

(defclass cache-map (hash-map lazy-cache-var) ())


