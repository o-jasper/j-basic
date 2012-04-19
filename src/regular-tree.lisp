;;
;;  Copyright (C) 19-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :regular-tree
  (:use :common-lisp :alexandria)
  (:export match-form apply-vars apply-vars-macro)
  (:documentation
   "Sort-of regular expressions, but for trees of objects."))

(in-package :regular-tree)

;;TODO a macro/compile version too?
(defun match-form (form expr &key vars (eql #'eql) compile (also-name t))
  "Returns assoc list variables if variables can be set to make form equal to\
 expr. Otherwise it returns a keyword stating what went wrong.

TODO FUNCALL undertested. LAMBDA not implemented (and feels dangerous)

Keyword compile currently does nothing, but may in future mean a\
 compiler-macro might see if it can 'compile' a way to match the form."
  (declare (ignore compile))
  (labels ;TODO Function is too long? At least it consists of subfunctions..
      ((mf-on-args (form expr &optional optional)
	 (when (null form) ;Done, check if too many arguments.
	   (unless (null expr)
	     (return-from match-form (values :too-few-args form expr vars)))
	   (return-from mf-on-args))
	 (when (and form (null expr) ;If not optional check enough arguments.
		    (not (or optional (eql (car form) :optional))))
	   (return-from match-form (values :too-many-args form expr vars)))
	 (case (car form) 
	   (:list ;TODO anything wrong with &optional/&rest ?
 	    (mf-on-args (cddr form) (mf-on-list (cadr form) expr) optional))
	   (:optional
	    (mf-on-args (cdr form) expr t)) ;TODO :key too?
	   (t
	    (mf (car form) (car expr))
	    (mf-on-args (cdr form) (cdr expr) optional))))
       (mf-on-list (form expr &key got)
	 (prog1
	     (do ((iter expr (cdr iter))) ((null iter) iter)
	       (assert (not (eql(car iter) :list)) nil "Already in list.")
	       (let ((m (match-form form (car iter) :also-name also-name)))
		 (unless (listp m) (return iter)) ;Mismatch.
		 (push m got)))
	   (push (append (list :list form) (reverse got)) vars)))
       (add-var (var-name value)
	 ;If already, exist, must be same
         (if-let (assoc (assoc var-name vars)) 
	   (unless (equalp (cadr assoc) value)
	     (return-from match-form 
	       (values :var-inconsistent var-name value vars)))
	   (push (list var-name value) vars))) ;Didn't exist, add it.
       (mf (form expr)
	 "Main matcher function."
	 (flet ((mis-match (name)
		  (return-from match-form 
		    (values name form expr vars))))
	   (typecase form
	     (null 
	      (when expr (mis-match :form-too-small)))
	     (symbol
	      (add-var form expr))
	     ((cons (eql funcall) (cons symbol list))
	      (typecase expr
		((cons (eql funcall) list)
		 (add-var (cadr form) (cadr expr))
		 (mf-on-args (cddr form) (cddr expr)))
		(list
		 (add-var (cadr form) (list 'function (car expr)))
		 (mf-on-args (cddr form) (cdr expr)))
		(t (mis-match :val))))
	     (list
	      (destructuring-bind (form-name &rest form-args) form
		(unless (listp expr)
		  (mis-match :fun))
		(destructuring-bind (expr-name &rest expr-args) expr
		  (cond (also-name
			 (mf-on-args form expr))
			((not (eql form-name expr-name))
			 (mis-match :fun-name))
			(t
			 (mf-on-args form-args expr-args))))))
	     (t ;Some object.
	      (unless (funcall eql form expr)
		(mis-match :val)))))
	 (values)))
    (mf form expr))
  vars)

(defun to-lisp-1 (expr)
  "Treat as lisp-1, making all the names of the expressions variables."
  (typecase expr
    (null nil)
    (list (cons 'funcall (mapcar #'to-lisp-1 expr)))
    (t    expr)))

;;TODO make a macro-version that produces code to do this.
(defun apply-vars (expr vars &key compile (also-name t))
  "Fills symbols matching to vars with the values of the vars.

Keyword compile currently does nothing, but may in future mean a\
 compiler-macro will see if it can 'compile' a way to apply the vars."
  (declare (ignore compile))
  (labels
      ((av (expr vars)
	 (typecase expr
	   (null    nil) ;TODO also deal with :list
	   (symbol  (or (cadr (assoc expr vars)) expr))
	   (list    (let ((args (mapcar (rcurry #'av vars) (cdr expr))))
		      (cond
			((and (eql (car expr) 'funcall) (listp (car args)))
			 (case (caar args)
			   (function
			    (assert (symbolp (cadar args)))
			    (cons (cadar args) (cdr args)))
			   (lambda 
			     (av (caddar args) 
			         (mapcar #'list (cadar args) (cdr args))))))
			(t
			 (cons (if also-name (car expr) 
				             (av (car expr) vars))
			       args)))))
	   (t       expr))))
    (av expr vars)))

(defmacro apply-vars-macro (expr &rest vars)
  "Apply the variables in a macro-manner. TODO currently only subset."
  (labels ((avm (expr vars)
	     (typecase expr
	       (null   nil)
	       (symbol (if-let (a (assoc expr vars))
			 (cadr a) expr))
	       (list   `(cons ',(car expr) ;TODO implement FUNCALL/LAMBDA
			      (list ,@(mapcar (rcurry #'avm vars)
					      (cdr expr)))))
	       (t      expr))))
    (avm expr vars)))
