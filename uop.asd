(in-package :asdf-user)
(defsystem "uop"
  :description "UOP object persistence with tags, groups, dynamic relationships"
  :version "0.0.1"
  :author "Samantha Atkins <sjatkins@gmail.com"
  :license "All Rights Reserved"
  :serial t
  :depends-on(#:postmodern #:alexandria #:sja #:uop-meta)
  :components
  (
   (:file "packages")
   (:file "utlis")
   (:file "changeset")
   (:file "db-collection")
   (:file "query")
   (:file "db")
   (:file "db-postmodern")))
