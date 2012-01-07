
(defsystem :test-j-parse-number
  :depends-on (:j-parse-number)
  :description "Bijection-tests parsing numbers on random generated input."
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "parse-number")))))
