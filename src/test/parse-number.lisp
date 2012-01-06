;;TODO apparently package-project of expression-hook doesn't catch that it is a test-package.

(defpackage :test-j-parse-number
  (:use :common-lisp :j-parse-number)
  (:export test))

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

(parse-number "-2352.35" :dot #\.)
 
