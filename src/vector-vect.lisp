;;
;;  Copyright (C) 12-04-2012 Jasper den Ouden.
;;  Placed in public domain. Do with it what you want, all 
;;   responsibilities by use and modification are yours.
;;

(defpackage :vector-vect
  (:use :common-lisp :alexandria)
  (:export vect2d vect3d x y z xyz xyzs mod-xyz mod-xyzs
	   v+ v- v* v/
	   inpr crosspr lensqr len normalize  distsqr dist
	   angle-v v2-angle rotate-v2 rotate-v3)
  (:documentation "Vectors from sequences.
Hope optimization will do its job well!"))

(in-package :vector-vect)

(deftype vect2d () '(vector number 2))
(deftype vect3d () '(vector number 3))

(defmacro mk-component (n name)
  "Simple utility to make the component functions."
  `(defmacro ,name (vect)
     ,(format nil "~D component of vector. (index ~D)" name n)
     `(aref ,vect ,',n)))

(mk-component 0 x)
(mk-component 1 y)
(mk-component 2 z)

(defmacro xyz (value &body body)
  "Makes settable x,y,z for you, or name-x,y,z"
  (with-gensyms (v)
    `(let ((,v ,value))
       (symbol-macrolet
	   (,@(loop for c in '(x y z)
		    for k from 0
		 collect `(,c (aref ,v ,k))))
	 ,@body))))

(defmacro mod-xyz ((name value) &body body)
  "Makes settable x,y,z for you, or name-x,y,z"
  (with-gensyms (v)
    `(let ((,v ,value))
       (symbol-macrolet
	   (,@(loop for c in '(x y z)
		    for k from 0
		 collect `(,(intern(format nil "~D-~D" name c))
			    (aref ,v ,k))))
	 ,@body))))

(defmacro xyzs (inputs &body body)
  "Multiple xyz macros in sequence."
  (if (null (cdr inputs))
    `(xyz ,(car inputs) ,@body)
    `(xyz ,(car inputs) (multi-xyz ,(cdr inputs) ,@body))))

(defmacro mod-xyzs (inputs &body body)
  "Multiple xyz macros in sequence."
  (if (null (cdr inputs))
    `(mod-xyz ,(car inputs) ,@body)
    `(mod-xyz ,(car inputs) (mod-xyzs ,(cdr inputs) ,@body))))

(defmacro with-make-array (var dim k &body body)
  `(let ((,var (make-array ,dim :initial-element 0)))
     (dotimes (,k ,dim) ,@body) ;Note abstraction leak on dim.
     ,var))

;;Adding, substracting.
(defun v+ (&rest vects)
  (reduce (lambda (a b)
	    (typecase a
	      (vector
	       (with-make-array out (length a) i
		 (setf (aref out i) (v+ (aref a i) (aref b i)))))
	      (number
	       (+ a b))))
	  vects))

(defun v* (vect &rest factors)
  "Multiplication by _scalars_"
  (let ((factor (reduce #'* factors)))
    (typecase vect
      (vector
       (with-make-array out (length vect) i
	 (setf (aref out i) (* (aref vect i) factor))))
      (number
       (* vect factor)))))

(defun v- (&rest vects)
  (v+ (car vects)
      (v* (apply #'v+ (cdr vects)) -1)))

(defun v/ (vect &rest divide-by)
  "Division by scalars."
  (let ((div (reduce #'* divide-by)))
    (assert (/= div 0))
    (v* vect (/ div))))

(defun inpr (&rest vals)
  "Inproducts and matrix multiplication."
  (reduce (lambda (a b)
	    (typecase a
	      (vector
	       (typecase b
		 (vector
		  (do*((i 0 (+ i 1))
		       (out (inpr (aref a 0) (aref b 0))
			    (v+ out (inpr (aref a i) (aref b i)))))
		      ((>= (+ i 1) (length a)) out)))
		 (number
		  (v* a b))))
	      (number
	       (* a b))))
	  vals))

(defun crosspr (a b)
  "Cross product. Only 2 or 3 dimensional."
  (mod-xyzs ((a a) (b b))
    (case (length a)
      (2 (- (* a-x b-y) (* a-y b-x)))
      (3 (vector (- (* a-y b-z) (* a-z b-y))
		 (- (* a-z b-x) (* a-x b-z))
		 (- (* a-x b-y) (* a-y b-x)))))))

(defun lensqr (&rest vects)
  "Length squared of vector."
  (loop for v in vects sum (inpr v v)))

(defun len (&rest vects)
  "Length of vector."
  (sqrt (apply #'lensqr vects)))

(defun normalize (v)
  (assert (/= (len v) 0))
  (v/ v (len v)))

(defgeneric distsqr (a b)
  (:documentation "Distance squared between two things."))
(defmethod distsqr ((a vector) (b vector))
  (lensqr (v- a b)))
(defgeneric dist (a b)
  (:documentation "Distance between two things."))
(defmethod dist (a b)
  (sqrt (distsqr a b)))

(defun angle-v-raw (angles &key pre)
  (if (null angles)
    `((* ,@pre))
    `((* ,@pre (cos ,(car angles)))
      ,@(angle-v-raw (cdr angles) :pre `(,@pre (sin ,(car angles)))))))

(defmacro angle-v (&rest angles)
  "Produces an n+1 dimensional vector from n angles."
  (let ((angle-vars (loop for el in angles
			 collect (gensym))))
    `(let (,@(loop for el in angles
		   for gs in angle-vars
		collect `(,gs ,el)))
       (vector ,@(angle-v-raw angle-vars)))))

(defun v2-angle (v)
  "Gets angle from 2d vector."
  (let ((len (len v)))
    (xyz v
      (if (= len 0) 0
	(* (acos (/ x len)) (if (< y 0) -1 1))))))

(defun rotate-v2 (v a &key (cos-a (cos a)) (sin-a (sin a)))
  "Rotates a 2d vector."
  (xyz v
    (vector (+ (* y sin-a) (* x cos-a))
	    (- (* y cos-a) (* x sin-a)))))

(defun rotate-v3 (v n a &key (cos-a (cos a)) (sin-a (sin a)))
  "Rotate 3d vector round something"
  (declare (type vect3d v n)
	   (type number a cos-a sin-a))
  (let*((e1 (xyz n
	      (normalize
	       (cond
		 ((and (= x 0) (= y 0))
		  (return-from rotate-v3
		    (xyz v
		      (vector (+ (* y sin-a) (* x cos-a))
			      (- (* y cos-a) (* x sin-a))
			      z)))) 
		 ((= x 0)
		  (vector 0d0 (/ -1 y) (/ z)))
		 ((= y 0)
		  (vector (/ -1 x) 0d0 (/ z)))
		 (t
		  (vector (/ x) (/ -1 y) 0d0))))))
	(e2 (normalize (crosspr e1 n)))
	(x (inpr e1 v))
	(y (inpr e2 v)))
    (declare (type vect3d e1 e2)
	     (type number x y))
    (v+ (v* e1 (+ (* y sin-a) (* x cos-a)))
	(v* e2 (- (* y cos-a) (* x sin-a)))
	(v* n  (inpr (normalize n) v)))))

(defun rotate-matrix-2 (a &key (cos-a (cos a)) (sin-a (sin a)))
  "Make the matrix that rotates"
  (declare (type number a cos-a sin-a))
  (vector (vector cos-a     sin-a)
	  (vector (- sin-a) cos-a)))

;TODO defun rotate-matrix-3, matrix stuff etcetera.

