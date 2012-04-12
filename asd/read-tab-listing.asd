
(defsystem :read-tab-listing
  :description "Reading and writing nested lists using indentation, and
 conversion between s/m-expressions.

Note that it can't do two consecutive lists, m-expressions are a bit of a
 work-around, but that it cannot do expressions without arguments.

Made initially to read the output if the wireless internet connection lister,
 iwlist.

TODO allow it to screen out comments."
  :serial t
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "read-tab-listing")))))
