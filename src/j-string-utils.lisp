;;
;;  Copyright (C) 27-01-2012 Jasper den Ouden.
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
	   tokenize tokenize-str; tokenize-modulo-quoted
	   keys-from-string
	   with-stream-generalized line-by-line with-line-by-line
	   rev-case string-rev-case string-case)
  (:documentation "Some basic stuff for strings.
 (connected to other projects, not good for general library use unless you're\
 willing to suffer changes.)"))

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
  (concat-list
   (map 'list (lambda (el) (if (stringp el) el (princ-to-string el)))
	sequence)))

(defun concat-princ (&rest sequence)
  "&rest version of concat-princ-list."
  (concat-princ-list sequence))

(defun wh (ch)
  (case ch ((#\Space #\Newline) t)))

(defun tokenize-str
    (string &optional (stop-at-string "") (wh #'wh) (upto-n -2))
  "Tokenizes string.
TODO from lib? TODO phase this guy out.
TODO bit poor choice of optional arguments, keywords would be better."
  (if-let (p (when (/= upto-n 0) (position* wh string)))
    (let*((rest (when-let (np (position*-not wh string :start p))
		  (tokenize-str (subseq string np) 
				stop-at-string wh (- upto-n 1))))
	  (ss (subseq string 0 p)))
      (if (or (= p 0) (string= ss stop-at-string))
	  rest (cons ss rest)))
    (if (string= string "") (list) (list string))))

(defmacro if-return-from (block-name &body (if-returned))
  (with-gensyms (var)
    `(when-let (,var ,if-returned) (return-from ,block-name ,var))))

;;TODO all the things that work on characters now should work on strings.
(defun tokenize (input &key (white (format nil "~%~T ")) 
		 (stop "") stop-while-open (singlets "") 
		 (ignore-start "") (ignore-end "") 
		 (start 0)
		 (open "") (close "") (assert-match t)
		 keep-ignore-p)
  "Tokenizes a stream, `white` indicate what is considered whitespace,
`open` and `close` are characters that open/close sublists.
`stop` indicates what characters indicate you should stop. `stop-while-open` 
indicates if to also stop while open.(defaultly not)"
  (labels
      ((maybe-string (cur-string n)
	 (unless (= n 0) 
	   (let ((have (get-output-stream-string cur-string)))
	     (unless (string= have "") (list have)))))
       (cont (stream cur-string closer n)
	 `(,@(maybe-string cur-string n)
	   ,@(tkz stream closer)))
       (read-comment (stream)
	 (with-output-to-string (s)
	   (do ((ch (read-char stream nil :eof) 
		    (read-char stream nil :eof)))
	       ((find (read-char stream nil :eof) ignore-end) nil)
	     (write-char ch s))))
       (tkz (stream closer)
	 (with-output-to-string (cur-string)
	   (do ((ch (read-char stream nil :eof) (read-char stream nil :eof))
		(n 0 (+ n 1)))
	       (nil nil)
	     (cond ;Stop for stopping characters (maybe)unless nested.
	       ((or (when (find ch stop) (or stop-while-open (= closer -1)))
		    (eql ch :eof))
		(return-from tkz (maybe-string cur-string n)))
	       ((find ch white) ;Skip over 'white' characters.
		(return-from tkz (cont stream cur-string closer n)))
	       ((find ch ignore-start) ;Skip until you see the end.
		(when keep-ignore-p
		  (return-from tkz 
		    `((,keep-ignore-p ,(read-comment stream))
		      ,@(cont stream cur-string closer n))))
		(do () ((find (read-char stream nil :eof) ignore-end) nil))
		(return-from tkz (cont stream cur-string closer n)))
	       ((find ch singlets) ;Enter 'singlets as single characters.
		(return-from tkz 
		  `(,@(maybe-string cur-string n)
		    ,ch
		    ,@(tkz stream closer))))
	       (t
		(or (when-let (i (position ch open))
		      (return-from tkz `(,@(maybe-string cur-string n)
				  ,(cons ch (tkz stream i))
				  ,@(tkz stream closer))))
		    (when-let (i (position ch close))
		      (assert (or (not assert-match) (= i closer)))
		      (return-from tkz (maybe-string cur-string n))))))
	   ;'just some character', put it in the string. 
	   ; (That is what al the `return-froms` are for..)
	     (write-char ch cur-string)))))
    (typecase input
      (stream (dotimes (k start) (read-char input nil nil)) 
	      (tkz input -1))
      (string (with-input-from-string (s (subseq input start)) (tkz s -1)))
      (t      (error "tokenize currently only does strings and streams.")))))

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
    (let ((list nil))
      (do () (nil nil)
	(let ((line (read-line stream nil :eof)))
	  (when (eql line :eof) (return))
	  (multiple-value-bind (result stop) (funcall function line)
	    (when stop (return))
	    (push result list))))
      (nreverse list))))

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
