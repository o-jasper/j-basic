
(defsystem :regular-tree
  :depends-on (:alexandria)
  :description "Sort-of regular expressions, but for trees of objects."
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "regular-tree")))))
