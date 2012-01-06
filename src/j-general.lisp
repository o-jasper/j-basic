;;
;;  Copyright (C) 05-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :j-general
  (:use :common-lisp :alexandria)
  (:export setf-
	   destructuring-lambda constantly*
	   delist enlist
	   alist plist-after plist class-plist
	   package-keyword
	   from-homedir file-extension sub-path-of)
  (:documentation "Some general little additions"))

(in-package :j-general)

(defmacro setf- (fun set &rest args)
  `(setf ,set (,fun ,set ,@args)))

(defun delist (x) (if (listp x) (car x) x))
(defun enlist (x) (if (listp x) x (list x)))

(defun describe-to-string (object)
  (with-output-to-string (s) (describe object s)))

(defmacro destructuring-lambda (args &body body)
  "Lambda with a destructuring bind on _first_ argument."
  (with-gensyms (a)
    `(lambda (,a)
       (destructuring-bind ,args ,a
         ,@body))))

(defmacro constantly* (value)
  "Like `constantly`, except that the output is executed each time."
  (with-gensyms (ignored)
    `(lambda (&rest ,ignored) (declare (ignore ,ignored)) ,value)))

(defun alist-fun (vars)
  (multiple-value-bind (vars package)
      (if (keywordp (car vars)) (values (cdr vars) (find-package (car vars))) vars)
    (mapcar (lambda (v) 
	      (multiple-value-bind (var val)
		  (typecase v
		    (symbol	                 (values v v))
		    ((cons (eql :a) list)        (values (cadr v) (caddr v)))
		    ((cons t (cons symbol list)) (values (cadr v) v))
		    (t                           (error "Can't make key for ~a" 
							v)))
		`(',(if package (intern (symbol-name var) package) var) ,val)))
	    vars)))

(defmacro alist (&rest vars)
  "Put in values, this will make a alist with the indicating symbols based on the\
 code of given values.

If the values are variables, the variable name will be given, if it is an\
 s-expression, the first argument is used. `(:a key value)` explicitly sets the\
 key. Otherwise, it produces an (macroexpand-time)error.

If the first argument is a keyword, it will make all the symbols for the plist that
package. (For instance `:keyword` to turn it into a 'regular' plist)"
  `(list ,@(mapcar (curry #'cons 'list) (alist-fun vars))))

(defmacro plist-after (after &rest vars)
  `(,@after ,@(mapcan #'identity (alist-fun vars))))

(defmacro plist (&rest vars)
  "This will make a plist with the indicating symbols based on the code of given\
 value.
To see how see the macro `alist`."
  `(plist-after (list) ,@vars))

(defmacro class-plist (class &rest vars)
  "This will make a class instance with the initargs based on the code of given\
 value.
To see how see the macro `alist`."
  `(plist-after (make-instance ,class) ,@vars))

(defun package-keyword (sym)
  (intern (if (packagep sym)
	    (package-name sym) 
	    (package-name (find-package sym))) :keyword))

;;These three functions here belong elsewhere.
(defun from-homedir (str)
  "Go from home directory."
  (concatenate 'string (namestring (user-homedir-pathname)) str))

(defun file-extension (file)
  "Gets file extension."
  (let ((file (if (pathnamep file) (file-namestring file) file)))
    (if-let (i (position #\. file :from-end t))
      (values (subseq file i) (subseq file 0 i))
      (values "" ""))))

(defun sub-path-of (path with)
  (let ((i (position #\/ path))
	(j (position #\/ with)))
    (cond
      ((or (not i) (not j))
       (= (length path) 0))
      ((and (= i j) (string= (subseq path 0 i) (subseq with 0 j)))
       (sub-path-of (subseq path (+ i 1)) (subseq with (+ j 1)))))))
