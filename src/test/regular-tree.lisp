;;
;;  Copyright (C) 19-04-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :regular-tree-test
  (:use :common-lisp :alexandria :gen-expr :regular-tree :j-seq-utils)
  (:export match-form-apply-vars-test specific-match-test test)
  (:documentation "Tests regular-tree."))

(in-package :regular-tree-test)

(defun chop-tree (tree &key (chop-prob 0.2) vars)
  "Chops bits off the tree and replaces them with variables. Later we see if\
 we can get those variables back."
  (labels ((ct (tree)
	     (cond
	       ((< (random 1.0) chop-prob) (with-gensyms (g)
					     (push (list g tree) vars)
					     g))
	       ((listp tree)               (cons (car tree)
						 (mapcar #'ct (cdr tree))))
	       (t                          tree))))
    (let ((res (ct tree)))
      (values res vars))))

;;Tests.
(defun match-form-apply-vars-test (&key (expr (gen-expr :fun-prob 0)))
  "Tests that (match-form expr (apply-vars expr vars)) should produce the\
 correct vars back."
  (multiple-value-bind (chopped vars) (chop-tree expr)
    (let ((matched-vars (match-form chopped expr)))
      (assert (listp matched-vars) nil "The form and expression didnt match.")
      (dolist (v matched-vars)
	
	(assert (or (and (null (assoc (car v) vars)) 
			 (eql (car v) (cadr v)))
		    (equalp (assoc (car v) vars) v) nil
		"Variable value of ~s didnt match ~s vs ~s"
		(car v) v vars)))
      (assert (equalp expr (apply-vars chopped vars)) nil
	      "Applying the variables back didnt get the original expression\
 back!"))))

(defun specific-match-tests (&rest cases)
  "Test specific examples.
TODO actual do apply-vars.."
  (let ((apply-test t))
    (dolist (case cases)
      (setq apply-test (case case (:apply t) (:dont-apply nil) 
			     (t apply-test)))
      (when (listp case)
	(destructuring-bind (form expr asserted-vars) case
	  (let ((result (match-form form expr :also-name nil)))
	    (assert (equalp result asserted-vars) nil
		    "vars didnt match ~s vs ~s" result asserted-vars)
	    (when apply-test
	      (assert (equalp expr (apply-vars form asserted-vars))))))))))

(defun test (&key (inverse-test-count 100))
  "Run tests."
  (maptimes inverse-test-count
	    (lambda (n) 
	      (declare (ignore n)) 
	      (match-form-apply-vars-test)))
  (specific-match-tests 
   '((a x) (a (b 1 2 3 4))
     ((X (B 1 2 3 4))))
   '((funcall a 3 4) (+ 3 4)
     ((a #'+)))
   '((q a x a) (q 1 2 1)
     ((x 2) (a 1)))
   :dont-apply ;Applying doesnt do :list, :optional yet!!
   '((f ka :optional opt) (f 1)
     ((opt nil) (ka 1)))
   '((a :list (+ q r) w) (a (+ 1 2) (+ 3 4) 9)
     ((W 9) (:LIST (+ Q R) ((R 2) (Q 1)) ((R 4) (Q 3)))))
   '((f a b c) (f q r) :too-many-args))
  :test-done)

(test)
