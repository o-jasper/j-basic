;;
;;  Copyright (C) 06-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :test-j-string-utils
  (:use :common-lisp :alexandria :j-string-utils :j-seq-utils :j-general)
  (:export test-tokenize test-search)
  (:documentation
   "Currently bijection-tests for random generated `detokenize`. (And `cl:search`)"))

(in-package :test-j-string-utils)

(defun detokenize (list)
  (typecase (car list)
    (string (concat (car list) " " (detokenize (cdr list))))
    (null   "")
    (list   (concat "\"" (detokenize (car list)) "\" " (detokenize (cdr list))))))

(defun random-str (&optional (len (+ 2 (random 5))))
  (coerce (maptimes len (constantly* (random-from "abcdefghijklmnopqrstuvwxyz")))
	  'string))

(defun test-tokenize (&key (n 100) (m 10))
  "Test plain tokenizing"
  (dotimes (k n)
    (let*((list (maptimes (random 10) (constantly* (random-str))))
	  (str  (detokenize list)))
      (assert (equalp (tokenize-str str) list) nil
	      "Test failed on: ~a != ~a (~a)" list (tokenize-str str) str))))

(test-tokenize)
    
(defun test-search (&key (n 100))
  "used to have a `locate` command, but now use cl:search, didn't feel like\
 throwing out test."
  (dotimes (k n)
    (let*((var (format nil "~a" (random 10000)))
	  (str (format nil "~a~a~a" (random 10000) var (random 10000))))
      (assert (search var str) nil "Could not find ~s in ~s" var str))))

(test-search)
