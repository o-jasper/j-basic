
(defsystem :test-read-tab-listing
  :depends-on (:read-tab-listing :gen-expr)
  :description "Uses :gen-expr to generate expressions of the subset that can
 be unabiguously read as tab-listing(those with at least one argument), then
 writes to string and reads it, checking equality. (Bijection test)"
    :license "GPLv3"
  :serial t
  :components ((:module "src"
                 :components ((:file "read-tab-listing")))))
