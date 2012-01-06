;;
;;  Copyright (C) 05-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :j-parse-number
  (:use :common-lisp :alexandria)
  (:export parse-number)
  (:documentation "`parse-integer`, but for numbers. "))

(in-package :j-parse-number)

(defun parse-integer-potential-e 
    (string &key (start 0)
     (i (position-if (lambda (ch) (not(digit-char-p ch))) string :start start))
     junk-allowed)
  (multiple-value-bind (num len) 
      (parse-integer string :start start :end (or i (length string)))
    (if (and i (case (aref string i) ((#\e #\E #\d #\D) t)))
      (values num len (parse-integer string :start (+ i 1) 
				     :junk-allowed junk-allowed))
      (values num len 0))))

(defun parse-positive-number-raw (string &key is-dot junk-allowed)
  "Raw version, parse-positive-number."
  (declare (type string string)
	   (type (function (character) boolean) is-dot)
	   (type boolean junk-allowed))
  (if-let (i (position-if
	      (lambda (ch)
		(or (funcall is-dot ch)
		    (case ch 
		      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) nil)
		      ((#\e #\E #\d #\D) t)
		      (t (assert junk-allowed nil "Junk in ~s" string)
			 t))))
	      string))
    (cond
      ((funcall is-dot (aref string i)) ;Is a real.
       (multiple-value-bind (result length exp);Junk should be users fault.
	   (parse-integer-potential-e string :start (+ i 1) 
				      :junk-allowed junk-allowed)
	 (* (expt 10d0 exp)
	    (+ (parse-integer string :end i) ;Shouldn't see junk.
	       (* result (expt 10d0 (- i -1 length)))))))
      (t
       (multiple-value-bind (result len exp)
	   (parse-integer-potential-e string :junk-allowed junk-allowed :i i)
	 (declare (ignore len))
	 (* result (expt 10d0 exp)))))
    (parse-integer string :junk-allowed junk-allowed)))

(defun parse-positive-number
    (string &key (start 0) end
                 (dot #\.) (is-dot (curry #'char= dot)) junk-allowed)
  "Documentation string refuses to be tautological."
  (if (digit-char-p (aref string start))
    (parse-positive-number-raw
     (if end
       (subseq string start end)
       (subseq string
	       (or (when junk-allowed
		     (position-if-not
		      (rcurry #'find '(#\Newline #\Space #\Tab)) string
		      :start start))
		   start)))
     :junk-allowed junk-allowed :is-dot is-dot)
    (progn (assert junk-allowed nil "~s starts with junk." string)
	   nil)))

(defun parse-number
    (string &key (start 0) end
                 (dot #\.) (is-dot (curry #'char= dot)) junk-allowed)
  "Not tautology for this one either!"
  (assert (> (length string) start) nil "~s is shorter than ~s."
	  string start)
  (flet ((parse-on (step)
	   (parse-positive-number string
	     :start (+ start step) :end end :is-dot is-dot
	     :junk-allowed junk-allowed)))
    (case (aref string start)
      (#\+ (parse-on 1))
      (#\- (- (parse-on 1)))
      (t   (parse-on 0)))))
