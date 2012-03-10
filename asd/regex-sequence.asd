;;DEPRECIATED

(defsystem :regex-sequence
  :depends-on (:regex)
  :description "Provides `destructuring-regex`, and using regex on lists of
 strings, allowing you to get integers marking the positiions, or the strings
 matches.

NOTE speed could probably be improved with compiler-macros and, more
 importantly, improving `destructuring-regex`.

NOTE 1) there is cl-ppcre too, how do they compare? Can i make it up to the
 user which to choose?
     2) how much does compiling the matchers cost? Can we memoize the
 results?"
    :license "GPLv3"
  :serial t
  :components ((:module "../src"
                 :components ((:file "regex-sequence")))))
