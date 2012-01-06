;;
;;  Copyright (C) 23-12-2011 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :read-tab-listing
  (:use :common-lisp)
  (:export read-tab-listing write-tab-listing
	   m-to-s-expr s-to-m-expr)
  (:documentation "Reading and writing nested lists using indentation, and\
 conversion between s/m-expressions.

Note that it can't do two consecutive lists, m-expressions are a bit of a\
 work-around, but that it cannot do expressions without arguments.

Made initially to read the output if the wireless internet connection lister,\
 iwlist.

TODO allow it to screen out comments."))

(in-package :read-tab-listing)

(defun m-to-s-expr (list)
  "Turn 'm-expressions' to s-expressions. 
Useful with read-tab-listing."
  (cond
    ((null (cdr list))
     list)
    ((listp(cadr list))
     (cons (cons (car list) (m-to-s-expr (cadr list)))
	   (m-to-s-expr (cddr list))))
    (t
     (cons (car list) (m-to-s-expr (cdr list))))))

(defun s-to-m-expr (list)
  "Turn m to s-expressions."
  (mapcan (lambda (el)
	    (if (listp el)
	      (list (car el) (s-to-m-expr (cdr el)))
	      (list el))) list))

(defun mapwhile (function) ;TODO 
  (let ((r (funcall function)))
    (when r
      (cons r (mapwhile function)))))

(defun read-tab-listing 
    (stream &key (tab-d 4) (hook #'identity) first-string
     m-to-s-expr comment-start
     (comment-p (if comment-start
		  (lambda (line) 
		    (unless (< (length line) (length comment-start))
		      (string= line comment-start :end1 (length comment-start))))
		  (constantly nil))))
  "Reads a level of deeper listing for deeper levels of tabbing/spacing."
  (declare (type fixnum tab-d) (type function hook))
  (multiple-value-bind (stream change) 
      (typecase stream (string   (values (make-string-input-stream stream) t))
		       (pathname (values (open stream) t))
		       (t        stream))
    (let ((cur first-string))
      (labels ((rl ()
		 (if cur 
		   (prog1 cur (setq cur nil))
		   (do ((line (read-line stream nil :eof)
			      (read-line stream nil :eof)))
		       ((or (eql line :eof) (not(funcall comment-p line)))
			line))))
	       (data-start (str)
		 (do ((i 0 (+ i 1))
		      (d 0 (+ d (case (aref str i) 
				  (#\Space 1) (#\Tab tab-d) (t 0)))))
		     ((or (>= i (length str))
			(case (aref str i)
			  ((#\Space #\Tab) nil)
			  (#\Newline       (error "Bug: should not find newline."))
			  (t               t)))
		      (values i d str))))
	       (wsd (&key (cur-d 0) (input (rl)))
		 "Deals with current level."
		 (unless (eql input :eof)
		   (multiple-value-bind (i d str) (data-start input)
		     (cond
		       ((= d cur-d) (funcall hook (subseq str i)))
		       ((> d cur-d) (cons (funcall hook (subseq str i))
					  (mapwhile (lambda () (wsd :cur-d d)))))
		       (t           (setq cur str)
				    nil))))))
	(prog1 (if m-to-s-expr (m-to-s-expr (mapwhile #'wsd)) (mapwhile #'wsd))
	  (when change (close stream)))))))

(defun write-tab-listing
    (list &key (stream *standard-output*) (d 0) (step 2) (format "~s~%"))
  "Use whitespace to indicate how the lists are listed."
  (declare (type list list))
  (mapcar (lambda (el)
	    (typecase el
	      (list (write-tab-listing el :stream stream :d (+ d step) :step step
				       :format format))
	      (t    (dotimes (k d) (write-char #\Space stream))
		    (format stream format el))))
	  list)
  (values))
