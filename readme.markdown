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
* (symbol string) go to match and put it in the variable.
* (symbol symbol) figure out a regular expression from the symbol using 
  `(regex-string-of-type type-symbol)`, and then parse it with 
  `(parse-type type-symbol string)` and put that in variable.
* `(:if-mismatch ..body..)`, `(t ..body..)` what to do if mismatch.

There is also a `regex-case` which picks the first matching 
`destructuring-regex` form. (Note that it is conceptually a little different 
that what `alexandria:destructuring-case` does!)

### J-parse-number
Just a little better than the other parse-number. read-tab-listing could be
useful. destructuring-regex uses it and provides a `parse-type` of
`(eql :num)` (and aliasses) for it.

### Regular-tree

A function `match-form form expr &key ...` where `form` is nested lists where
symbols mean variables. The function returns a assoc list when the variables 
match.(and various things 'what went wrong' otherwise)

The opposite `apply-vars form vars &key ...` can fill in variables as given 
in the `vars` assoc list.

### The little-lackings:

* vector-vect, vectors using `(vector..)`, adding inproduct
* j-generic, general stuff(not really used, but i might.)
* j-seq-utils, some stuff like making `position*`, `find*`, more general 
  versions.
* j-string-utils, some stuff for convenient string stuff, like
  (potentially nested) tokenizing, `concat`, `string-rev-case`.
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

### TODO

* Make a test for destructuring-regex. Also make a pcre version.

* Make the test for nested tokenizing more thurrough.

* Regular-tree `:optional` and `:list` keywords dont work with `apply-vars` 
  yet. There is no `:key` yet either.(And why did i not start these with `&`?
  
* Regular-tree could use some concepts from destructuring regex,
  'continuing on elements'. For instance, when it sees the correct things in
  `form` and `expr` in the right spaces, it could use
  `destructuring-regex:regex-list`.

## Copyright
Some of it is under the public domain, and some under the GPLv3, license
included under `doc/`

## Author
Jasper den Ouden
