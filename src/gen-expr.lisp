;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;  Placed in public domain. Do with it what you want, all 
;;   responsibilities by use and modification are yours.
;;

(defpackage :gen-expr
  (:use :common-lisp :alexandria)
  (:export gen-expr)
  (:documentation "Generates random expressions for data to test on."))

(in-package :gen-expr)

(defun random-from (list)
  "Get random file from the list."
  (nth (random (length list)) list))

(defun maptimes (fun n)
  "Run function n times, listing the output.(n as argument.)"
  (when (> n 0)
    (cons (funcall fun) (maptimes fun (- n 1)))))

(defconstant +from-symbols+ 
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
  "List of symbols to generate defaultly from.")

(defun gen-expr
    (&key (from-funs +from-symbols+) (gen-fun (curry #'random-from from-funs))
          (from-vars +from-symbols+) (gen-var (curry #'random-from from-vars))
     (var-prob 0.4) (val-prob 0.01) (fun-prob 0.2)
     (val-lower (+ var-prob val-prob)) (fun-lower (+ val-lower fun-prob))
     (min-args 0) (max-args 4)
     (arg-count (lambda ()
		  (+ min-args (random (- max-args min-args)))))
     (max-depth 4) environment 
     (gen-val (lambda ()
		(case (random 2)
		  (0 (random 10.0)) (1 (random 10))))))
  "Creates valid statements if the functions exist and don't care about the number\
 of arguments."
  (labels
      ((ge (&optional environment (depth 0) (r (random 1d0)))
	 (cond
	   ((and environment (\= var-prob 0) (< r var-prob)) ;Generate variable.
	    (random-from environment))
	   ((or (< r val-lower) (>= depth max-depth)) ;Generate value.
	    (funcall gen-val))
	   ((and (\= fun-prob 0) (< r fun-lower))
	    (let ((args (maptimes gen-var (funcall arg-count))))
	      `(lambda ,args
		 ,(ge (union environment args) (+ depth 1)))))
	   (t ;Generate function.
	    (cons (funcall gen-fun)
		  (maptimes (curry #'ge environment (+ depth 1)) 
			    (funcall arg-count)))))))
    (ge environment)))
