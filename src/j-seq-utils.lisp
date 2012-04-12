;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;  Placed in public domain. Do with it what you want, all 
;;   responsibilities by use and modification are yours.
;;

(defpackage :j-seq-utils
  (:use :common-lisp :alexandria)
  (:export flatten maptimes
	   position* position*-not find* find*-not
	   subseq-upto subseq-upto-not subseq-upfrom subseq-upfrom-not
	   seq-neql seq-eql
	   random-from
	   with-assocs
	   sublisting-tokens)
  (:documentation "Extra functions for sequences.
 (connected to other projects, not good for general library use unless you're\
 willing to suffer changes.)"))

(in-package :j-seq-utils)

(defun maptimes (n function) ;
  "Do function `n` times, listing results. You get the current number as\
 index. (loopless has something like it.)"
  (labels ((mt (i)
	     (when (< i n) (cons (funcall function i) (mt (+ i 1))))))
    (mt 0)))

(defgeneric set-test (item)
  (:documentation
   "If you want a predicate function, this makes anything that can be taken\
 as one; a single item(equality), a list/hash table (equality with one of\
 them), a function(the function itself taken as predicate.")
  (:method ((list list))
    (curry #'find list))
  (:method ((hash-table hash-table))
    (curry #'gethash hash-table))
  (:method ((function function))
    function)
  (:method (item)
    :eql)) ;Signal.

(defun position* (item seq &key (start 0) end from-end (fun (set-test item)))
  "Find position of function/item/element-in-list."
  (case fun
    (:eql (position item seq :start start :end end
		    :from-end from-end))
    (t    (position-if fun seq :start start :end end :from-end from-end))))

(defun position*-not (item seq &key (start 0) end from-end 
		      (fun (set-test item)))
  "Find position where function/item/element-in-list is not."
  (position* nil seq :start start :end end :from-end from-end
	     :fun (compose #'not 
			   (case fun (:eql (curry #'eql item)) (t fun)))))

(defun find* (item seq &key (start 0) end from-end (fun (set-test item)))
  "Find position of function/item/element-in-list."
  (case fun
    (:eql (find item seq :start start :end end
		    :from-end from-end))
    (t    (find-if fun seq :start start :end end :from-end from-end))))

(defun find*-not (item seq &key (start 0) end from-end (fun (set-test item)))
  "Find position where function/item/element-in-list is not."
  (find* nil seq :start start :end end :from-end from-end
	 :fun (compose #'not (case fun (:eql (curry #'eql item)) (t fun)))))

(defun subseq-upto (item seq &key (start 0) end from-end)
  "Take the subseq upto an element belonging to the set implied by the item."
  (if-let (p (position* item seq :start start :end end :from-end from-end))
    (subseq seq 0 p) seq))

(defun subseq-upto-not (item seq &key (start 0) end from-end); (one 1))
  "Take the subseq upto an element belonging to the set _not_ implied by the\
 item."
  (if-let (p (position*-not item seq :start start :end end 
			    :from-end from-end))
    (subseq seq 0 (+ p 1)) seq))

(defun subseq-upfrom (item seq &key (start 0) end from-end (one 1))
  "Take the subseq upfrom an element belonging to the set implied by the\
 item."
  (if-let (p (position* item seq :start start :end end :from-end from-end))
    (subseq seq (+ p one)) seq))

(defun subseq-upfrom-not (item seq &key (start 0) end from-end (one 0))
  "Take the subseq upfrom an element belonging to the set _not_ implied by\
 the item."
  (if-let (p (position*-not item seq 
			    :start start :end end :from-end from-end))
    (subseq seq (+ p one)) seq))

(defun subseq-between (a b seq &key (start 0) end)
  "Subsection between two things."
  (let ((seq (if end (subseq seq start end) (subseq seq start))))
    (subseq-upto b (subseq-upfrom a seq))))

(defun seq-eql (seq1 seq2 &key (test #'eql) (start1 0) (end1 (length seq1))
		(start2 0) range1 
		(end2 (if range1 (+ start2 (- end1 start1)) (length seq2))))
  "Equality of two sequences."
  (declare (type fixnum start1 end1 start2 end2))
  (when (= (- end1 start1) (- end2 start2))
    (map nil (lambda (a b)
	       (unless (funcall test a b) (return-from seq-eql nil)))
	 (subseq seq1 start1 end1) (subseq seq2 start1 end1))
    t))

(defun seq-neql (seq1 seq2 &key (test #'eql) (start1 0) (end1 (length seq1))
		 (start2 0) range1
		 (end2 (if range1 (+ start2 (- end1 start1)) (length seq2))))
  "Inequality of two sequences, returns where  it is unequal in the first\
 one."
  (let ((i start1))
    (map nil (lambda (a b)
	       (unless (funcall test a b) (return-from seq-neql i))
	       (setq i (+ i 1)))
	 (subseq seq1 start1 end1) (subseq seq2 start2 end2))
    (unless (= (- end1 start1) (- end2 start2)) i)))

(defun random-from (sequence)
  "Get element randomly from sequence."
  (elt sequence (random (length sequence))))

(defmacro with-assocs (names alist &body body)
  "Gets the `cdr` of `assoc` into variables, setf-ably."
  (let ((vars (mapcar (lambda (ignore) (declare (ignore ignore)) (gensym))
		      names))
	(al (gensym)))
    `(let ((,al ,alist))
       (let ,(mapcar (lambda (v name) 
		       `(,v (cdr(assoc ',(if (listp name) (car name) name)
				       ,al))))
		     vars names)
	 (symbol-macrolet 
	     ,(mapcar
	       (lambda (v name &key (onto (if (listp name) (cadr name) name)))
		 `(,(if (keywordp onto) (intern (symbol-name onto)) onto) ,v))
	       vars names)
	   ,@body)))))

(defun sublisting-tokens 
    (list start-end  &key cur-start (cur-end (cdr(assoc cur-start start-end)))
     allow-p)
  "The given things in start-end are used to make sublists. The starter is\
 used to indicate which it is. TODO move to j-basic; j-seq-utils"
  (assert (not (and cur-start cur-end)))
  (labels 
      ((sw (list cur-end)
	 (unless (null list)
	   (destructuring-bind (car &rest cdr) list
	     (let ((end (cadr (assoc car start-end))))
	       (cond (end
		      (multiple-value-bind (sub cont-sub) (sw cdr end)
			(multiple-value-bind (after cont-after) 
			    (sw cont-sub cur-end)
			  (values (cons (cons car sub) after) cont-after))))
		     ((eql car cur-end) ;It is the current end.
		      (values nil cdr))
		     ((and (not allow-p)
			   (let ((found (find-if
					 (lambda (el) (eql car (cdr el)))
					 start-end)))
			     (and found (not (caddr found)))))
		      (error "Expected end ~s got ~s" cur-end car))
		     (t 
		      (multiple-value-bind (ret cont) (sw cdr cur-end)
			(values (cons car ret) cont)))))))))
    (sw list cur-end)))
