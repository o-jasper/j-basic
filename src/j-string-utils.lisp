;;
;;  Copyright (C) 05-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :j-string-utils
  (:use :common-lisp :alexandria :j-seq-utils)
  (:export start-str= end-str= 
	   concat-list concat  concat-princ-list concat-princ
	   tokenize-str; tokenize-modulo-quoted
	   keys-from-string
	   with-stream-generalized line-by-line with-line-by-line
	   rev-case string-rev-case string-case)
  (:documentation "Some basic stuff for strings."))

(in-package :j-string-utils)

(defun start-str= (str eql-to)
  "string= the first argument upto the length of the second argument."
  (unless (< (length str) (length eql-to))
    (string= str eql-to :end1 (length eql-to))))

(defun end-str= (str eql-to)
  "string= at the end of the string."
  (unless (< (length str) (length eql-to)) 
    (string= str eql-to :start1 (- (length str) (length eql-to)))))

(defun concat-list (sequences)
  "Append all the strings in the list;\
 `(apply (curry #'concatenate 'string) sequences)`"
  (declare (type list sequences))
  (apply (curry #'concatenate 'string) sequences))

(defun concat (&rest sequences)
  "&rest version of concat-list."
  (concat-list sequences))

(defun concat-princ-list (sequence)
  (declare (type list sequence))
  "Print the elements into strings and concatenate them."
  (concat-list (map 'list (lambda (el) (if (stringp el) el (princ-to-string el)))
		    sequence)))

(defun concat-princ (&rest sequence)
  "&rest version of concat-princ-list."
  (concat-princ-list sequence))

(defun wh (ch)
  (case ch ((#\Space #\Newline) t)))

(defun tokenize-str (string &optional (stop-at-string "") (wh #'wh) (upto-n -2))
  "Tokenizes string.
TODO from lib? 
TODO bit poor choice of optional arguments, keywords would be better."
  (if-let (p (when (/= upto-n 0) (position* wh string)))
    (let*((rest (when-let (np (position*-not wh string :start p))
		  (tokenize-str (subseq string np) 
				stop-at-string wh (- upto-n 1))))
	  (ss (subseq string 0 p)))
      (if (or (= p 0) (string= ss stop-at-string))
	  rest (cons ss rest)))
    (if (string= string "") (list) (list string))))

(defun tokenize-modulo-quoted (str)
  "Tokenize, but keep together quoted with \" bits. TODO probably has bugs.."
  (if-let (p (position* (lambda (ch) (case ch ((#\" #\Space #\Tab #\Newline) t)))
			str))
    (cons (subseq str 0 p)
	  (case (elt str p)
	    (#\"
	     (if-let (p2 (position #\" str :start (+ p 1)))
	       (cons (subseq str p p2) 
		     (tokenize-modulo-quoted (subseq str (+ p2 1))))
	       (list (subseq str p))))
	    ((#\Space #\Tab #\Newline)
	     (tokenize-modulo-quoted
	      (subseq-upfrom-not #'wh (subseq str p) :one 0)))))
    (list str)))

(defun keys-from-string (string &optional (package :keyword))
  "Turn the tokenized elements of a string into symbols.(default: keywords)"
  (mapcar (lambda (str) (intern (string-upcase str) package))
	  (tokenize-str string)))

;;TODO replace with `with-input` and `with-output`?
(defmacro with-stream-generalized (stream &body body)
  "'Generalize' the stream, turns pathnames, strings into streams for you.
`stream` _must_ be a variable."
  (with-gensyms (b)
  `(flet ((,b (,stream) ,@body))
     (typecase ,stream
       (stream   (,b ,stream))		     
       (pathname (with-open-file (,stream ,stream) (,b ,stream)))
       (string   (with-input-from-string (,stream ,stream) (,b ,stream)))
       (keyword  (case ,stream
		   ((:string-output :str-out)
		    (with-output-to-string (,stream) (,b ,stream)))
		   (:stdout
		    (,b *standard-output*))
		   (:stdin
		    (,b *standard-input*))
		   (t
		    (error "`with-stream-generalized`: don't recognize ~a.
 (At runtime)"
			   ,stream))))))))

(defun line-by-line (stream function)
  "Pass line by line in stream/string.\
Runs until second value non-nil or end of stream."
  (with-stream-generalized stream
    (let ((line (read-line stream nil :eof)))
      (unless (eql line :eof)
	(multiple-value-bind (result stop) (funcall function line)
	  (unless stop
	    (cons result (line-by-line stream function))))))))

(defmacro with-line-by-line (stream (line) &body body)
  "Macro version of `line-by-line`, gives you `read-line` in sequence.
Use second value non-nil to stop.(Otherwise to end of stream.)"
  `(line-by-line ,stream (lambda (,line) ,@body)))


(defun string-rev-case (string)
  "Reverse the case of a string."
  (declare (type string string))
  (map 'string (lambda (ch)
		 (if (upper-case-p ch) (char-downcase ch) (char-upcase ch))) 
       string))

(defun rev-case (string)
  "Use `string-rev-case` instead."
  (string-rev-case string))

(defmacro string-case (str &body clauses)
  "Basic implementation of string cases."
  (with-gensyms (s)
    (labels ((handle-condition (cond &optional allow-list)
	       (typecase cond
		 ((cons string list)
		  (assert allow-list nil
			  "May not have two lists nested in string-case")
		  `(or ,@(mapcar #'handle-condition cond)))
		 ((eql t)
		  t)
		 (t
		  `(string= ,cond ,s))))
	     (handle-clause (c)
	       `(,(handle-condition (car c) t) ,@(cdr c))))
    `(let ((,s ,str))
       (cond ,@(mapcar #'handle-clause clauses))))))
