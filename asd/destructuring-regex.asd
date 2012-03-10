
(defsystem :destructuring-regex
  :depends-on (:j-parse-number :alexandria :regex)
  :description "Provides `destructuring-regex`, and using regex on lists of
 strings, allowing you to get integers marking the positions, or the strings
 matches.

Also provides a way of extending it and a concept that should have come with the
 Common Lisp language or a standard or at least widely used library; `parse-type`
 and `regex-string-of-type`"
  :serial t
  :license "GPLv3"
  :components ((:module "../src"
                 :components ((:file "destructuring-regex")))))
