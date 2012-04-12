
(defsystem :denest
  :depends-on (:alexandria)
  :description "Macro to denest, remove nestedness of macros
/functions. Was somewhat of a relevation to me and suprising that this
 function isn't being screamed from the rooftops. Turns out that macros
 like iterate might just be trying to fight nestedness.(Mostly)

A few macros are given, some are on keywords and specific to denest, to 
 save namespace.  They do _exactly_ the same as regular macros however! You
 can use them with the USE-DENEST macro. The macros supplied in this packageIf
with non-keyword symbols are exported.

TODO remove some nastyness that might happen if denest is nested with self..
Can it be done?

Note: i hardly use it myself!! Better not use it.
TODO suggest improved ones of these to loopless? Particularly the
 accumulators."
  :serial t
  :license "Public domain"
  :author "Jasper den Ouden"
  :components ((:module "../src"
                 :components ((:file "denest")))))
