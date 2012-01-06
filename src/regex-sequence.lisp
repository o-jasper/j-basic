;;
;;  Copyright (C) 23-12-2011 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :regex-sequence
  (:use :common-lisp :regex)
  (:export regex-list regex-subdivide destructuring-regex)
  (:documentation "Provides `destructuring-regex`, and using regex on lists of\
 strings, allowing you to get integers marking the positiions, or the strings\
 matches.

NOTE speed could probably be improved with compiler-macros and, more\
 importantly, improving `destructuring-regex`.

NOTE 1) there is cl-ppcre too, how do they compare? Can i make it up to the\
 user which to choose?
     2) how much does compiling the matchers cost? Can we memoize the\
 results?"))

(in-package :regex-sequence)

(defun regex-list (reg-list string &key (start 0) (stop t))
  "Makes a list of where it matched, and from where to where. The matches must\
 be after each other. (elements are lists from-to)"
  (when reg-list
    (destructuring-bind (cur &rest rest) reg-list
      (multiple-value-bind (result from len)
	  (cond ((not (stringp cur))
		 (scan-str cur string :start start))
		((string= cur "")
		 (values t start 0))
		((or (string= cur "(") (string= cur ")"))
		 (let ((i (search cur string)))
		   (when i (values t i 1))))
		(t
		 (let ((c (compile-str cur)))
		   (assert c nil "Couldn't get compiled string of ~a" cur)
		   (scan-str c string :start start))))
	(cond
	  (result (cons (list from (+ from len)) 
			(regex-list rest string 
				    :start (+ from len) :stop stop)))
	  (stop   nil) ;Couldn't find any more.
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

(defun regex-subdivide (reg-list string &key (start 0) (stop t) (with-first t))
  "Uses the regular expressions to subdivide the string.\
 (Trivial combination of `pair-subdiv` and `regex-list`)"
   (pair-subdiv (regex-list (if with-first (cons "" reg-list) reg-list)
			    string :start start :stop stop)
		string))

(defmacro destructuring-regex (regs+vars string &body body)
  "Finds the regular expresions in sequence and puts the values in. reg+vars`:
* if string, just match, ignore any values.
* Otherwise first element an expression that returns a string, then optionally\
 variables to put the between-matched-and-next/next, string-matched.
* Any variable can be declared 'boring' by making it :b

TODO (string var) sends the match to var, and just val "
  (let (boring (subdiv (gensym)))
    (labels ((meh () (car (push (gensym) boring)))
	     (handle-var (reg+var)
	       (cons '&optional
		     (typecase reg+var
		       ((or null keyword)
			(error "No meaning for ~a at the moment.." reg+var))
		       (symbol
			`(,(meh) ,reg+var))
		       (string
			`(,(meh) ,(meh)))
		       ((cons t (cons symbol null))
			`(,(cadr reg+var) ,(meh)))
		       ((cons t list)
			`(,(caddr reg+var) ,(cadr reg+var)))
		       (t
			(error "`destructuring-regex` has no meaning with ~a" 
			       reg+var)))))
	     (handle-regex (reg+var)
	       (typecase reg+var
		 ((or null keyword)
		  (error "No meaning for ~a at the moment.." reg+var))
		 (string reg+var)
		 (list   (car reg+var))
		 (symbol ""))))
      `(let*((,subdiv (regex-subdivide 
		       (list ,@(mapcar #'handle-regex regs+vars))
		       ,string :with-first nil))
	     (,subdiv (if (< (length ,subdiv) ,(length regs+vars))
			(append ,subdiv (make-list (- ,(length regs+vars)
						      (length ,subdiv))))
			,subdiv)))
	 (destructuring-bind ,(mapcar #'handle-var regs+vars) ,subdiv
	   (declare (ignore ,@boring))
	   ,@body)))))
	 
