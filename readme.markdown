# j-Basic
Most of these are to get rid of 'little lackings' of the lisp language; except:

### destructuring-regex
Uses [regex](http://www.cliki.net/REGEX) to provide `destructuring-bind` for
regular expressions.
`(destructuring-regex regex-and-vars string ..body..)` where `regex-and-vars` 
consists of:

* symbols, the value entered is whatever is between other matching
* strings is skip to a match, dont record the match 
  (but maybe record what is between)
* (string symbol) go to match and put it in the variable.
* (symbol symbol) figure out a regular expression from the symbol using 
  `(regex-string-of-type type-symbol)`, and then parse it with 
  `(parse-type type-symbol string)` and put that in variable.
* `(:if-mismatch ..body..)` what to do if mismatch.

### J-parse-number
Just a little better than the other parse-number. read-tab-listing could be useful.
destructuring-regex uses it and provides a `parse-type` of  `(eql :num)`
(and aliasses) for it.

## Notes
Dependencies are [alexandria](http://common-lisp.net/project/alexandria),
 regex-sequence also depends on [regex](http://www.cliki.net/REGEX).

## Copyright
Everything is under GPLv3, license included under `doc/`

## Author

Jasper den Ouden
