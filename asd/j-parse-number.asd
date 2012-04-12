
(defsystem :j-parse-number
  :depends-on (:alexandria)
  :description "`parse-integer`, but for numbers. "
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "parse-number")))))
