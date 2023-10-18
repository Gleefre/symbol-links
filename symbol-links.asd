(defsystem "symbol-links"
  :description "Hack that enables symbol-links on sbcl"
  :version "0.0.1"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :pathname "src"
  :components ((:file "package")
               (:file "symbol-links-base")
               (:file "symbol-links-plus")
               #+sbcl (:file "sbcl-hack")
               #+lispworks (:file "lispworks-hack")
               #+ccl (:file "ccl-hack")))
