
(defsystem :regular-tree-test
  :depends-on (:j-seq-utils :regular-tree :gen-expr :alexandria)
  :description "Tests regular-tree"
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "regular-tree")))))
