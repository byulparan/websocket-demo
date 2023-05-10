
(defpackage #:mat
  (:use #:cl #:ps)
  (:shadow #:identity)
  (:import-from :gl :define-ps-macro)
  (:export #:install-matrix
	   #:mv-push-matrix
	   #:mv-pop-matrix
	   #:set-matrix-uniforms))

(in-package :mat)

(define-ps-macro create-mat4 ()
  `(chain gl-matrix mat4 (create)))

(define-ps-macro create-mat3 ()
  `(chain gl-matrix mat3 (create)))

(define-ps-macro set-mat4 (src-matrix dst-matrix)
  `(chain gl-matrix mat4 (set ,src-matrix ,dst-matrix )))

(define-ps-macro identity (dest)
  `(chain gl-matrix mat4 (identity ,dest)))

(define-ps-macro perspective (fovy aspect near far dest)
  `(chain gl-matrix mat4 (perspective ,fovy ,aspect ,near ,far ,dest)))

(define-ps-macro ortho (left right bottom top near far dest)
  `(chain gl-matrix mat4 (ortho ,left ,right ,bottom ,top ,near ,far ,dest)))

(define-ps-macro rotate (dest angle axis)
  `(chain gl-matrix mat4 (rotate ,dest ,dest ,angle ,axis )))

(define-ps-macro translate (dest vec)
  `(chain gl-matrix mat4 (translate ,dest ,dest ,vec)))

(define-ps-macro scale (dest vec)
  `(chain gl-matrix mat4 (scale ,dest ,vec)))

(define-ps-macro to-inverse-mat3 (src-matrix dst-matrix)
  `(chain gl-matrix mat4 (to-inverse-mat3 ,src-matrix ,dst-matrix)))

(define-ps-macro transpose (matrix)
  `(chain gl-matrix mat3 (transpose ,matrix)))



(defpsmacro install-matrix (shader-program p-matrix mv-matrix)
  `(let ((p-matrix-location (gl:get-uniform-location ,shader-program "uPMatrix"))
	 (mv-matrix-location (gl:get-uniform-location ,shader-program "uMVMatrix"))
	 (mv-matrix-stack (list)))
     (setf ,p-matrix (create-mat4))
     (setf ,mv-matrix (create-mat4))
     (js:define-jsfun mv-push-matrix ()
       (let* ((copy (create-mat4)))
	 (set-mat4 ,mv-matrix copy)
	 (chain mv-matrix-stack (push copy))))
     (js:define-jsfun mv-pop-matrix ()
       (setf ,mv-matrix (chain mv-matrix-stack (pop))))
     (js:define-jsfun set-matrix-uniforms ()
       (gl:uniform-matrix-4fv p-matrix-location f ,p-matrix)
       (gl:uniform-matrix-4fv mv-matrix-location f ,mv-matrix))))

;;; dummy
(defun install-matrix (shader-program p-matrix mv-matrix)
  (declare (ignore shader-program p-matrix mv-matrix)))

(defun mv-push-matrix ()
  ())

(defun mv-pop-matrix ()
  ())

(defun set-matrix-uniforms ()
  ())
