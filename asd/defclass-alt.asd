
(defsystem :defclass-alt
  :depends-on (:alexandria)
  :description "Just implements a defclass*"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "defclass-alt")))))
