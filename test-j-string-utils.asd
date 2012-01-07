
(defsystem :test-j-string-utils
  :depends-on (:j-general :j-seq-utils :j-string-utils :alexandria)
  :description "Currently bijection-tests for random generated `detokenize`. (And `cl:search`)"
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "j-string-utils")))))
