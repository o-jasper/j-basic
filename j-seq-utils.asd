
(defsystem :j-seq-utils
  :depends-on (:alexandria)
  :description "Extra functions for sequences."
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "j-seq-utils")))))
