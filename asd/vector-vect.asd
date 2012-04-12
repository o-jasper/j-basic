
(defsystem :vector-vect
  :depends-on (:alexandria)
  :description "Vectors from sequences.
Hope optimization will do its job well!"
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "vector-vect")))))
