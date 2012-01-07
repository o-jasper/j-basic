
(defsystem :gen-expr
  :depends-on (:alexandria)
  :description "Generates random expressions for data to test on."
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "gen-expr")))))
