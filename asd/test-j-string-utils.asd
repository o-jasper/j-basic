
(defsystem :test-j-string-utils
  :depends-on (:j-general :j-seq-utils :j-string-utils :alexandria)
  :description "Currently bijection-tests for random generated `detokenize`. (And `cl:search`)"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "j-string-utils")))))
