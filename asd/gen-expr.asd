
(defsystem :gen-expr
  :depends-on (:alexandria)
  :description "Generates random expressions for data to test on."
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "gen-expr")))))
