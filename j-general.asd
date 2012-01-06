
(defsystem :j-general
  :depends-on (:alexandria)
  :description "Some general little additions"
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "j-general")))))
