;;
;;  Copyright (C) 05-01-2012 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :defclass-alt
  (:use :common-lisp :alexandria)
  (:export defclass* enlist-instance*
	   *class*-hash* gather-slots
	   values-from-instance values-from-instance*
	   defclass*+methods)
  (:documentation "Just implements a defclass*"))

(in-package :defclass-alt)

(defgeneric enlist-instance* (obj)
  (:documentation "Turns a instance into an assoc-list of its slot."))

(defvar *class*-hash* (make-hash-table) "Hashtable with the classes.")

(defun delist (x) (if (listp x) (car x) x))

(defun gather-slots (of &key delist)
  "Gather all the slots of the class, including what it derives from."
  (typecase of
    (null   nil)
    (symbol (gather-slots (gethash of *class*-hash*) :delist delist))
    (t      (let ((ret (destructuring-bind (derive &rest slots) of
			 (remove-if #'stringp
				(append slots (mapcan #'gather-slots derive))))))
	      (if delist (mapcar #'delist ret) ret)))))


(defmacro defclass* (name derive-from &rest elements)
  "Define a class more conveniently.
an element is in form `name docs &optional type init`"
  (setf (gethash name *class*-hash*) (cons derive-from elements))
  `(progn 
     (defclass ,name ,derive-from
       (,@(mapcan
	   (lambda (el)
	     (typecase el
	       (list
		(destructuring-bind (name docs &optional type (init nil initp)
				     &key (reader :default) (initarg :default))
		    el
		  `((,name ,@(typecase reader 
			       ((eql :default) `(:reader ,name))
			       (null           nil)
			       (symbol         `(:reader ,reader)))
			   ,@(typecase initarg
			       ((eql :default) `(:initarg 
						 ,(intern (symbol-name name)
							  :keyword)))
			       (null            nil)
			       (symbol         `(:initarg ,initarg)))
			   ,@(when initp `(:initform ,init))
			   :documentation ,docs 
			   ,@(when type `(:type ,type))))))
	       (symbol
		`((,el :reader ,el
		       :initarg ,(intern (symbol-name el) :keyword))))
	       (t
		nil))) 
	   elements))
       ,@(alexandria:when-let (doc (find-if #'stringp elements))
	   `((:documentation ,doc))))
     (defmethod enlist-instance* ((obj ,name))
       ,(let ((slots (remove-duplicates 
		      (remove-if-not #'symbolp
			(mapcar #'delist (gather-slots
					  (cons derive-from elements)))))))
	     `(with-slots (,@slots) obj
		(list ,@(mapcar (lambda (s) `(list ',s ,s)) slots)))))))

(defmacro defclass*+methods (name derive-from elements &body methods)
  "Define a class with `defclass*` and define methods right after."
  (setf (gethash name *class*-hash*) (cons derive-from elements))
  `(progn (defclass* ,name ,derive-from
	    ,@elements)
	  ,@(mapcar (lambda (el)
		      (typecase (cadr el)
			(list
			 `(defmethod ,(car el) ((,name ,name) ,@(cadr el))
			    (with-slots ,(gather-slots name :delist t) ,name
			      ,@(cddr el))))
			(symbol
			 `(defmethod ,(car el) ,(cadr el)
			    ((,name ,name) ,@(caddr el))
			    (with-slots ,(gather-slots name :delist t) ,name
			      ,@(cdddr el))))))
		    methods)))

(defun c/a (first values)
  "Cons or append (convenience)"
  (typecase first 
    (list (append first values))
    (null nil) 
    (t    (cons first values))))

(defun values-from-instance (instance &optional first &rest values)
  "Gets some of the values from an instance."
  (mapcar (curry #'slot-value instance) (c/a first values)))

(defmacro values-from-instance* (class instance &rest values)
  "Macro version of `values-from-instance`, if in *class*-hash* will check."
  (dolist (el values)
    (assert (symbolp el))
    (assert (find el (mapcar #'delist (gather-slots class)))))
  `(with-slots (,@values) ,instance (list ,@values)))
