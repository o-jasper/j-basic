
(defsystem :j-parse-number
  :depends-on (:alexandria)
  :description "`parse-integer`, but for numbers. "
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "parse-number")))))
