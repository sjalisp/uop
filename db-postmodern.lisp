; consider in-package with usage and exports.  Is "right thing"
(in-package :com.conceptwareinc.uop)

(defclass metaclass ()
  ((_id :col-type uuid :initarg :id :reader id)
   (system-defined :col-type boolean :initarg :system :reader system-defined)
   (userid :col-type string :initarg :userid :reader userid)
   (description :col-type string :initarg :description :accessor description)
   (name :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys _id))

(defclass metattr ()
  ((_id :col-type string :initarg :id :reader id)
   (userid :col-type uuid :initarg :userid :reader userid)
   (name :col-type string :initarg :name :accesor name)
   (type :col-type integer :initarg :type :accessor attr-type)
   (description :col-type string :initarg :description :accessor description)
   (modifiable :col-type boolean :initarg :modifiable :accessor modifiable :initform nil))
  (:metaclass dao-class)
  (:keys _id))

(defclass classattrs ()
  ((class-id :col-type uuid :initarg :class-id :reader attr-class)
   (attr-id :col-type uuid :initarg :attr-id :accessor attr-id)
   (order :col-type integer :initarg :order :accessor order))
  (:metaclass dao-class))

(defclass superclass ()
  ((class-id :col-type uuid :initarg :class-id :accessor subclass-id)
   (superclass :col-type uuid :initarg superclass :accessor superclass))
  (:metaclass dao-class))

(defclass metarole ()
  ((name :col-type string :initarg :name :accessor name)
   (_id :col-type uuid :initarg :id :reader id)
   (userid :col-type uuid :initarg :userid :reader userid)
   (reverse-id :col-type uuid :initarg :id :reader reverse-id)
   (description :col-type text :initarg :description :accessor description)
   (is-reverse :col-type boolean :initarg :id :reader is-reverse))
  (:metaclass dao-class)
  (:keys _id))

(defclass metatag ()
  ((name :col-type string :initarg :name :reader name)
   (userid :col-type uuid :initarg :userid :reader userid)
   (_id :col-type uuid :initarg :id :reader id)
   (description :col-type string :initarg description :accessor description))
  (:metaclass dao-class)
  (:keys _id))

(defclass metagroup ()
  ((name :col-type string :initarg :name :reader name)
   (userid :col-type uuid :initarg :userid :reader userid)
   (_id :col-type uuid :initarg :id :reader id)
   (description :col-type string :initarg description :accessor description))
  (:metaclass dao-class)
  (:keys _id))

(defclass app-user()
  ((last-name :col-type string :initarg :last-name :accesor last-name)
   (first-name :col-type string :initarg :first-name :accessor first-name)
   (email :col-type string :initarg :email :accessor email)
   (_id :col-type uuid :initarg :userid :accessor userid)
   (pasword :col-type password :initarg :password :accesor password)
   (username :col-type username :initarg :username :accessor username)
   (rel-table :col-type string :initarg :rel-table :accesor rel-table)
   (tag-table :col-type string :initarg :tag-table :accessor tag-table)
   (grp-table :col-type string :initarg :grp-table :accessor grp-table))
  (:metaclass dao-class)
  (:keys _id))

(defclass applicaiton ()
  ((_id :col-type uuid :reader id)
   (name :col-type string :accessor name)
   (url :col-type string :acessor url :initarg :url)
   (description :col-type text :accessor description :initarg :description))
  (:metaclass dao-class)
  (:keys _id))
   
(defclass appclass (metaclass)
  ((appid :col-type uuid :initarg: :appid))
  (:metaclass dao-class))

(defclass userapps ()
  ((userid :col-type uuid :initarg :userid)
   (appid :col-type uuid :initarg :appid))
  (:metaclass dao-class))

(defclass tagged ()
  ((tag-id :col-type string :initarg :tag-id :reader tag-id)
   (object :col-type string :initarg :object :reader object))
  (:metaclass dao-class))

(defclass grouped ()
  ((group-id :col-type string :initarg :group-id :reader group-id)
   (object :col-type string :initarg :object :reader object))
  (:metaclass dao-class))

(defclass related ()
  ((subject :col-type uuid :initarg :subject :reader subject)
   (role :col-type uuid :initarg :role :reader role)
   (object :col-type uuid :initarg :object :reader object))
  (:metaclass dao-class))



(defgeneric set-up-database(database))
(defgeneric create-meta_structure

(defmettod setup-types((db supported-database))
  (create-meta-structure db)
  (add-base-types (db))

(defclass postmodern-db (database))
  
(defmethod set-up-database ((db postmodern-db))
  
(defmethod open-db ((db postmodern-db) &optional user-id default-data)
  (set-up-database db)
  (load-metadata))

(defclass postmodern-collection (





