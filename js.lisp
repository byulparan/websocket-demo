
(defpackage #:js
  (:use #:cl #:ps)
  (:export #:define-ps-macro
	   #:define-jsfun
	   ;; dom
	   #:->
	   #:by-id
	   #:by-tagname
	   #:create-element
	   #:create-text-node))


(in-package #:js)

(defmacro define-ps-macro (name arg &body body)
  `(progn
     (defmacro ,name ,arg
       ,@body)
     (import-macros-from-lisp ',name)
     (export ',name)))

(defmacro define-jsfun (name args &body body)
  (let ((table-name (intern "*JS-TABLE*" *package*)))
    `(progn
       (defvar ,table-name nil)
       (setf (getf ,table-name ',name)
	     (quote (setf ,name (lambda ,args ,@body))))
       ',name)))

(defpsmacro define-jsfun (name args &body body)
  `(progn (setf ,name (lambda ,args ,@body))
	  "undefined"))


(defpsmacro -> (&body chain)
  `(chain ,@chain))

(defpsmacro by-id (id)
  `(-> document (get-element-by-id ,id)))

(defpsmacro by-tagname (tagname)
  `(-> document (get-elements-by-tag-name ,tagname)))

(defpsmacro create-element (tagname)
  `(-> document (create-element ,tagname)))

(defpsmacro create-text-node (text)
  `(-> document (create-text-node ,text)))
