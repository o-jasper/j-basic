
;Just some trying stuff.
(in-package :regex-sequence)

(destructuring-regex (start "a" a "b" b ("d" d) c)
    "..a1b24vvcd"
  (list start a b c d))

(destructuring-regex ("aa" ("[:digit:]++" a b)) "3466 aa 253 b"
  (list a b))

(destructuring-regex
    (name " " ("[:digit:]+" number)
     ": "("Full|Charging|Discharging|design capacity|on-line|\
ok|trip point|active|LCD|Processor|Fan" info) " |, " rest)
    "ska 5: Full, monkey"
  (list name number info rest))

(destructuring-regex (("[:digit:]+|[:digit:]+.[:digit:]+" val) p) "35.35"
  (list val p))

(regex-list '("[:digit:]+" "") "100%")
