# j-Basic
Most of these are to get rid of 'little lackings' of the lisp language; 
except:

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

There is also a `regex-case` which picks the first matching 
`destructuring-regex` form. (Note that it is conceptually a little different 
that what `alexandria:destructuring-case` does!)

### J-parse-number
Just a little better than the other parse-number. read-tab-listing could be useful.
destructuring-regex uses it and provides a `parse-type` of  `(eql :num)`
(and aliasses) for it.

### The little-lackings:

* vector-vect, vectors using `(vector..)`, adding inproduct
* j-generic, general stuff(not really used, but i might.)
* j-seq-utils, some stuff like making `position*`, `find*`, more general 
  versions.
* j-string-utils, some stuff for convenient string stuff, like tokenizing,
  `concat`, `string-rev-case`.
* j-parse-number, because i felt i could do better.
* gen-expr, generates random expressions. (for bijection tests for instance)
* denest, it is annoying when code gets nested. Just *de*nest is _a_ reaction.
  Though not the best one, usually improvement of code makes it go away 
  better.
* read-tab-listing, reads nested lists where the nestedness is indicated by 
  differing levels of indentation.

## Notes
Dependencies are [alexandria](http://common-lisp.net/project/alexandria),
 regex-sequence also depends on [regex](http://www.cliki.net/REGEX). 

## Copyright
Some of it is under the public domain, and some under the GPLv3, license
included under `doc/`

## Author

Jasper den Ouden
