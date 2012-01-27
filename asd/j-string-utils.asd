
(defsystem :j-string-utils
  :depends-on (:j-seq-utils :alexandria)
  :description "Some basic stuff for strings."
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "j-string-utils")))))
