;;
;;  Copyright (C) 27-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :test-read-tab-listing
  (:use :common-lisp :gen-expr :read-tab-listing)
  (:export test)
  (:documentation "Uses :gen-expr to generate expressions of the subset that can\
 be unabiguously read as tab-listing(those with at least one argument), then\
 writes to string and reads it, checking equality. (Bijection test)"))

(in-package :test-read-tab-listing)

(defun test (&key (n 100)
	     (vars (map 'list #'identity "abcdefghijklmnopqrstuvwxyz")))
  "Bijection test write-tab-listing and read-tab-listing.(See docstr package)"
  :bijection-test
  (dotimes (k n)
    (let ((expr (gen-expr :from-funs vars :from-vars vars
				   :min-args 1 :fun-prob 0)))
      (when (listp expr)
	(let ((bij (m-to-s-expr (s-to-m-expr expr))))
	  (assert (equalp expr bij) nil "s/m expression bijection failed at(~a):
~s~%~s" k expr bij))
	(with-input-from-string
	    (stream
	     (with-output-to-string (str-stream)
	       (write-tab-listing (s-to-m-expr expr) :stream str-stream :step 1)))
	  (let ((read (m-to-s-expr
		       (read-tab-listing stream :hook #'read-from-string))))
	    (assert (equalp read expr) nil
		    "Failed to read exactly at(~a) expr,read~%~s~%~s" 
		    k expr read)))))))

;(test)
