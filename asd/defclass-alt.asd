
(defsystem :defclass-alt
  :depends-on (:alexandria)
  :description "Just implements a defclass*
 (Now barely)connected to other projects, not good for general library use
 unless you're willing to suffer changes."
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "defclass-alt")))))
