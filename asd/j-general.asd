
(defsystem :j-general
  :depends-on (:alexandria)
  :description "Some general little additions"
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "j-general")))))
