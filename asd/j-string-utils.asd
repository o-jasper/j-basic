
(defsystem :j-string-utils
  :depends-on (:j-seq-utils :alexandria)
  :description "Some basic stuff for strings.
 (connected to other projects, not good for general library use unless you're
 willing to suffer changes.)"
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "j-string-utils")))))
