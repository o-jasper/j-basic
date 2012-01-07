;;
;;  Copyright (C) 06-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :test-j-parse-number
  (:use :common-lisp :j-parse-number)
  (:export test)
  (:documentation "Bijection-tests parsing numbers on random generated input."))

(in-package :test-j-parse-number)

(defun test (&key (fr -10.0) (to 10.0) (to-test #'parse-number)
	     (round-to 1d-6))
  "Bijection tests parse-number"
  (let ((x (round (+ fr (random (- to fr))) round-to)))
    (assert (= (coerce x 'double-float)
	       (funcall to-test (princ-to-string x)))
	    nil "Test failed ~a ~a" (coerce x 'double-float)
      (funcall to-test (format nil "~a" x)))))

(dotimes (k 100)
  (test :fr -732525325.0 :to 71213532532.0))

(assert (= (parse-number "-2352.35" :dot #\.) -2352.35d0))
 
