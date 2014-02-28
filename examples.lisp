
(defpackage #:web-repl
  (:use #:cl #:websock #:js #:ps))

(in-package #:web-repl)

(defparameter *server* (make-instance 'hunchentoot:easy-acceptor :port 8080))
(hunchentoot:start *server*)

(start-websocket-server 9999) ;start-websocket server


(hunchentoot:define-easy-handler (ws-repl :uri "/ws-repl.html") nil
  (who:with-html-output-to-string (html nil :prologue "<!doctype html>")
    (:html
     (:head
      (:title "web-repl demo"))
     (:body
      (:script (ps:ps-to-stream html (install-websock 9999)))))))

;;; open your webbrowser. then go to 127.0.0.1:8080/ws-repl.html

(in-ws-repl
  (alert "hello world"))

(in-ws-repl
  (+ 10 20 30 40))

(in-ws-repl
  (define-jsfun foo (a b c)
    (+ a b c)))

(in-ws-repl
  (foo 10 20 30))

(in-ws-repl
  (let ((canvas (create-element "canvas")))
    (setf (@ canvas style background) "black")
    (-> canvas (set-attribute "id" "gl-canvas"))
    (-> canvas (set-attribute "width" (@ window inner-width)))
    (-> canvas (set-attribute "height" (@ window inner-height)))
    (-> (@ document body) (append-child canvas))))

(in-ws-repl
  (let ((script (create-element "script")))
    (setf (-> script src) "https://glmatrix.googlecode.com/files/glMatrix-0.9.5.min.js")
    (-> document body (append-child script))))


(defparameter *vertex-shader* "
attribute vec3 aVertexPosition;
attribute vec3 aVertexColor;
uniform mat4 uPMatrix;
uniform mat4 uMVMatrix;
varying highp vec4 uColor;
void main () {
gl_Position =  uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
uColor = vec4(aVertexColor, 1.0);
}")

(defparameter *fragment-shader* "
varying highp vec4 uColor;
void main () {
gl_FragColor = uColor;
}")


;;; setup for webgl
(in-ws-repl
  (gl:with-gl-canvas (canvas "gl-canvas")
    (setf shader-program (gl:init-shader (lisp *vertex-shader*)
					 (lisp *fragment-shader*)))
    (gl:use-program shader-program)
    (mat:install-matrix shader-program p-matrix mv-matrix)))

(in-ws-repl
  (setf (@ shader-program vertex-pos-attrib) (gl:get-attrib-location shader-program "aVertexPosition")
	(@ shader-program vertex-color-attrib) (gl:get-attrib-location shader-program "aVertexColor"))
  (gl:enable-vertex-attrib-array (@ shader-program vertex-pos-attrib))
  (gl:enable-vertex-attrib-array (@ shader-program vertex-color-attrib))
  (setf vertex-position-buffer (gl:create-buffer)
	vertex-color-buffer (gl:create-buffer)
	vertex-indicies-buffer (gl:create-buffer)))

;;; init buffers
(in-ws-repl
  (gl:bind-buffer :array-buffer vertex-position-buffer)
  (gl:buffer-data :array-buffer (new (-float-32-array (list -.5 -.5 0
							    .5 -.5 0
							    .5 .5 0
							    -.5 .5 0))) :static-draw)
  (gl:bind-buffer :array-buffer vertex-color-buffer)
  (gl:buffer-data :array-buffer (new (-float-32-array (list (random) (random) (random)
							    (random) (random) (random)
							    (random) (random) (random)
							    (random) (random) (random)))) :static-draw)
  (gl:bind-buffer :element-array-buffer vertex-indicies-buffer)
  (gl:buffer-data :element-array-buffer (new (-uint-16-array (list 0 1 3
								   2 3 1))) :static-draw))


;;; definition degree-to-radian
(in-ws-repl
  (setf *degree* 0.0)
  (define-jsfun deg-to-rad (degree)
    (/ (* degree pi) 180.0)))

;;; draw
(in-ws-repl
  (define-jsfun draw-scene ()
    (let ((bg-color (abs (sin (* .5 (deg-to-rad *degree*))))))
      (gl:clear-color bg-color bg-color bg-color 1.0))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0 0 (@ canvas width) (@ canvas height))
    (mat:perspective 45 (/ (@ canvas width) (@ canvas height)) .1 100. p-matrix)
    (mat:identity mv-matrix)
    (mat:translate mv-matrix (list -10. 0 -20.))
    (gl:bind-buffer :array-buffer vertex-position-buffer)
    (gl:vertex-attrib-pointer (@ shader-program vertex-pos-attrib) 3 :float f 0 0)
    (gl:bind-buffer :array-buffer vertex-color-buffer)
    (gl:vertex-attrib-pointer (@ shader-program vertex-color-attrib) 3 :float f 0 0)
    (gl:bind-buffer :element-array-buffer vertex-indicies-buffer)
    (incf *degree* 1)
    (dotimes (i 20)
      (mat:rotate mv-matrix (deg-to-rad *degree*) (list 1 0 1))
      (mat:translate mv-matrix (list 1 0 0))
      (mat:set-matrix-uniforms)
      (gl:draw-elements :triangles 6 :unsigned-short 0))))


;;; animate!
(in-ws-repl
  (define-jsfun animate ()
    (draw-scene)
    (request-animation-frame animate))
  (animate))


