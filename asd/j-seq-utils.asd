
(defsystem :j-seq-utils
  :depends-on (:alexandria)
  :description "Extra functions for sequences.
 (connected to other projects, not good for general library use unless you're
 willing to suffer changes.)"
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "j-seq-utils")))))
