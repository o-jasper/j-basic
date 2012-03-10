;;
;;  Copyright (C) 10-03-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :destructuring-regex
  (:use :common-lisp :regex :alexandria :j-parse-number)
  (:export destructuring-regex parse-type regex-string-of-type
	   regex-list regex-subdivide)
  (:documentation "Provides `destructuring-regex`, and using regex on lists of\
 strings, allowing you to get integers marking the positions, or the strings\
 matches.

Also provides a way of extending it and a concept that should have come with the\
 Common Lisp language or a standard or at least widely used library; `parse-type`\
 and `regex-string-of-type`"))

(in-package :destructuring-regex)

(defun regex-list (reg-list string &key (start 0) (stop t))
  "Makes a list of pairs indexes, indicating where it matches. 
If keyword `stop` true, the first mismatch stops the process."
  (declare (type fixnum start) (type list reg-list))
  (when reg-list
    (destructuring-bind (cur &rest rest) reg-list
      (multiple-value-bind (have-match-p from len)
	  (cond ((not (stringp cur)) ;Assumes it is a compiled regex.
		 (scan-str cur string :start start))
		((string= cur "")
		 (values t start 0))
		((or (string= cur "(") (string= cur ")"));TODO this does what?
		 (let ((i (search cur string)))
		   (when i (values t i 1))))
		(t
		 (let ((regexer (compile-str cur))) ;Compile regex.
		   (assert regexer nil "Couldn't get compiled string of ~a" cur)
		   (scan-str regexer string :start start))))
	(cond
	  (have-match-p ;Found one, continue.
	          (cons (list from (+ from len))
			(regex-list rest string 
				    :start (+ from len) :stop stop)))
	  (stop   nil) ;Couldn't find it, and setting is to stop.
	  (t      (cons nil (regex-list rest string
					:start start :stop nil))))))))

(defun pair-subdiv (list seq)
  "Subdivide sequence by given pairs returning pairs\
 (inside-pair between-pair)"
  (when list
    (destructuring-bind ((from to) &rest rest) list
      (cons (list (subseq seq from to) 
		  (if rest (subseq seq to (caar rest)) (subseq seq to)))
	    (pair-subdiv rest seq)))))

(defun regex-subdivide(reg-list string &key (start 0) (stop t) (with-first t))
  "Uses the regular expressions to subdivide the string.\
 (Trivial combination of `pair-subdiv` and `regex-list`)"
   (pair-subdiv (regex-list (if with-first (cons "" reg-list) reg-list)
			    string :start start :stop stop)
		string))

(defgeneric regex-string-of-type (type)
  (:documentation "Returns a regular expression string for a given symbol.
Note: If you make 'types' indicated by symbols you 'dont own', symbols do not\
 indicate dependencies!")
  (:method ((tp (eql 'integer)))  "[+-]?[0-9]+")
  (:method ((tp (eql :int)))      (regex-string-of-type 'integer))
  (:method ((tp (eql 'unsigned))) "[+]?[0-9]+")
  (:method ((tp (eql :uint)))     (regex-string-of-type 'unsigned))
  (:method ((tp (eql :uint*)))    "[0-9]+")
  (:method ((tp (eql :pos-int)))  (regex-string-of-type 'unsigned))
  (:method ((tp (eql :pos-int*))) "[0-9]+")
  (:method ((tp (eql 'number)))   "[+-]?[0-9]+\.?[0-9]+")
  (:method ((tp (eql :num)))      (regex-string-of-type 'number))
  (:method ((tp (eql :pos-num)))  "[+]?[0-9]+\.?[0-9]+")
  (:method ((tp (eql :pos-num*)))  "[0-9]+\.?[0-9]+"))

(defgeneric parse-type (type string) ;TODO &key (start 0) end junk-allowed
  (:documentation "Parse something of some type.")
  (:method ((tp (eql 'integer)) (string string))  
    (parse-integer string :junk-allowed nil))
  (:method ((tp (eql 'unsigned)) (string string))
    (parse-integer string :junk-allowed nil))
  (:method ((tp (eql 'number)) (string string))
    (parse-number string :junk-allowed nil))
  (:method ((tp (eql :int)) (string string))      (parse-type 'integer string))
  (:method ((tp (eql :uint)) (string string))     (parse-type 'unsigned string))
  (:method ((tp (eql :uint*)) (string string))    (parse-type 'unsigned string))
  (:method ((tp (eql :num)) (string string))      (parse-type 'number string))
  (:method ((tp (eql :num*)) (string string))     (parse-type 'number string))
  (:method ((tp (eql :pos-num)) (string string))  (parse-type 'number string))
  (:method ((tp (eql :pos-num*)) (string string)) (parse-type 'number string)))

;;TODO remake it scanning just a bit for each var.
(defmacro destructuring-regex ((&rest regs+vars) input-string &body body)
  "Finds the regular expresions in sequence and puts the values in. reg+vars`:
* If non-keyword symbol, it fills a role as 'inbetween' variable. 
  Only one at a time allowed.
* if string, just match, ignore any values
* (string symbol) puts the match into a variable of symbol
* (symbol symbol) will match using `regex-string-of-type` and then parse using\
  `parse-type`"
  (with-gensyms (result from len new-i first-i string)
    (labels
	((if-between (between i)
	   (when between ;Fill between-string.
	     `((,between (subseq ,string ,i ,from)))))
	 (dr (var vars i body &optional if-mismatch between)
	   (typecase var
;TODO keywords indicating stuff?
	     (null
	      `(,@(if between `(let ((,between (subseq ,string ,i)))) '(progn))
		  ,@body))
	     (symbol ;Variable to put stuff between to matches.
	      (assert (not between) nil 
		      "Can't get between-regexes twice. old ~s new ~s"
		      between var)
	      (dr (car vars) (cdr vars) i body if-mismatch var))
	     (string ;Skip string, fill between variable if provided.
	      `(multiple-value-bind (,result ,from ,len)
		   (scan-str (compile-str ,var) ,string :start ,i)
		 (when ,result
		   (let (,@(if-between between i)
			 (,new-i (+ ,from ,len)))
		     ,@(if vars (list (dr (car vars) (cdr vars) new-i body 
					  if-mismatch))
			        body)))))
	     ((cons (eql :if-mismatch) list)
	      (dr (car vars) (cdr vars) i body (cdr var) between))
	     ((cons symbol (cons symbol null)) ;Want that match.
	      `(multiple-value-bind (,result ,from ,len)
		   (scan-str (compile-str ,(regex-string-of-type (car var)))
			     ,string :start ,i)
		 (if ,result
		   (let*(,@(if-between between i)
			 (,new-i      (+ ,from ,len))
			 (,(cadr var) (parse-type ',(car var)
						  (subseq ,string ,from ,new-i))))
		     ,(dr (car vars) (cdr vars) new-i body if-mismatch))
		   ,if-mismatch)))
 ;TODO ((cons (or (eql integer) (eql :int)) (cons symbol null)))
 ;TODO ((cons (or (eql number)  (eql :num)) (cons symbol null)))
	     ((cons string (cons symbol null)) ;Want that match.
	      `(multiple-value-bind (,result ,from ,len)
		   (scan-str (compile-str ,(car var)) ,string :start ,i)
		 (if ,result
		   (let*(,@(if-between between i)
			 (,new-i (+ ,from ,len))
			 (,(cadr var) (subseq ,string ,from ,new-i)))
		     ,(dr (car vars) (cdr vars) new-i body if-mismatch))
		   ,if-mismatch)))
	     (t
	      (error "`destructuring-regex` does not have a meaning for ~s"
		     var)))))
      `(let ((,first-i 0) (,string ,input-string))
	 ,(dr (car regs+vars) (cdr regs+vars) first-i body)))))
