
(defpackage #:gl
  (:use #:cl #:js #:ps)
  (:export #:init-shader))

(in-package :gl)


;;;
(defun gl-type (type)
  (ecase type
    (:float '(chain gl *float*))
    (:unsigned-byte '(chain gl *unsigned_byte*))
    (:unsigned-short '(chain gl *unsigned_short*))))

(defun gl-format (format)
  (ecase format
    (:rgba '(chain gl *rgba*))))

(defun gl-draw-mode (mode)
  (ecase mode
    (:line-strip '(chain gl *line_strip*))
    (:line-loop '(chain gl *line_loop*))
    (:triangles '(chain gl *triangles*))
    (:triangle-strip '(chain gl *triangle_strip*))))

(defun gl-clear-mode (mode)
  (ecase mode
    (:color-buffer-bit '(chain gl *color_buffer_bit*))
    (:depth-buffer-bit '(chain gl *depth_buffer_bit*))))

(defun gl-enable-mode (mode)
  (ecase mode
    (:depth-test '(chain gl *depth_test*))
    (:blend '(chain gl *blend*) )))

(defun gl-blend-mode (mode)
  (ecase mode
    (:src-alpha '(chain gl *src_alpha*))
    (:one '(chain gl *one*))))

(defun gl-shader-mode (shader-mode)
  (ecase shader-mode
    (:vertex-shader '(chain gl *vertex_shader*))
    (:fragment-shader '(chain gl *fragment_shader*))))

(defun gl-program-status (status)
  (ecase status
    (:compile-status '(chain gl *compile_status*))
    (:link-status '(chain gl *link_status*))))

(defun gl-buffer-target (target)
  (ecase target
    (:array-buffer '(chain gl *array_buffer*))
    (:element-array-buffer '(chain gl *element_array_buffer*))))

(defun gl-buffer-usage (usage)
  (ecase usage
    (:static-draw '(chain gl *static_draw*))))

(defun gl-texture-unit (unit)
  (ecase unit
    (:texture0 '(chain gl *texture0*))
    (:texture1 '(chain gl *texture1*))
    (:texture2 '(chain gl *texture2*))
    (:texture3 '(chain gl *texture3*))))

(defun gl-texture-target (target)
  (ecase target
    (:texture-2d '(chain gl *texture_2-d*))))

(defun gl-texture-parameter (parameter)
  (ecase parameter
    (:texture-mag-filter '(chain gl *texture_mag_filter*))
    (:texture-min-filter '(chain gl *texture_min_filter*))
    (:texture-wrap-s '(chain gl *texture_wrap_s*))
    (:texture-wrap-t '(chain gl *texture_wrap_t*))
    (:unpack-flip-y-webgl '(chain gl *unpack_flip_y_webgl*))
    (:unpack-premultiply-alpha-webgl '(chain gl *unpack_premultiply_alpha_webgl*))
    (:unpack-colorspace-convension-webgl '(chain gl *unpack_colorspace_convension_webgl*))))

(defun gl-texture-param-name (name)
  (ecase name
    (:linear '(chain gl *linear*))
    (:nearest '(chain gl *nearest*))
    (:nearest-mipmap-nearest '(chain gl *nearest_mipmap_nearest*))
    (:linear-mipmap-nearest '(chain gl *linear-mimap-nearest*))
    (:nearest-mipmap-linear '(chain gl *nearest_mipmap_linear*))
    (:linear-mipmap-linear '(chain gl *linear_mipmap_linear*))
    (:repeat '(chain gl *repeat*))
    (:clamp-to-edge '(chain gl *clamp_to_edge*))
    (:mirrored-repeat '(chain gl *mirrored_repeat*))))

;;;;

(define-ps-macro enable (mode)
  `(chain gl (enable ,(gl-enable-mode mode))))

(define-ps-macro clear-color (r g b a)
  `(chain gl (clear-color ,r ,g ,b ,a)))

(define-ps-macro clear (&rest buffers)
  `(chain gl (clear (logior ,@(mapcar #'gl-clear-mode buffers)))))

(define-ps-macro viewport (x y width height)
  `(chain gl (viewport ,x ,y ,width ,height)))

(define-ps-macro blend-func (src target)
  `(chain gl (blend-func ,(gl-blend-mode src) ,(gl-blend-mode target))))

(define-ps-macro draw-arrays (mode first count)
  `(chain gl (draw-arrays ,(gl-draw-mode mode) ,first ,count)))

(define-ps-macro draw-elements (mode count type offset)
  `(chain gl (draw-elements ,(gl-draw-mode mode) ,count ,(gl-type type) ,offset)))

;;; Shader
(define-ps-macro create-program ()
  `(chain gl (create-program)))

(define-ps-macro link-program (program)
  `(chain gl (link-program ,program)))

(define-ps-macro use-program (program)
  `(chain gl (use-program ,program)))

(define-ps-macro create-shader (mode)
  `(chain gl (create-shader ,(gl-shader-mode mode))))

(define-ps-macro shader-source (shader src)
  `(chain gl (shader-source ,shader ,src)))

(define-ps-macro compile-shader (shader)
  `(chain gl (compile-shader ,shader)))

(define-ps-macro attach-shader (program shader)
  `(chain gl (attach-shader ,program ,shader)))

(define-ps-macro get-program-parameter (program pname)
  `(chain gl (get-program-parameter ,program ,(gl-program-status pname))))

(define-ps-macro get-shader-parameter (shader pname)
  `(chain gl (get-shader-parameter ,shader ,(gl-program-status pname))))

;;; inter-shader
(define-ps-macro get-uniform-location (program uniform)
  `(chain gl (get-uniform-location ,program ,uniform)))

(define-ps-macro uniform1i (location value)
  `(chain gl (uniform1i ,location ,value)))

(define-ps-macro uniform1f (location value)
  `(chain gl (uniform1f ,location ,value)))

(define-ps-macro uniform2f (location value1 value2)
  `(chain gl (uniform2f ,location ,value1 ,value2)))

(define-ps-macro uniform3f (location value1 value2 value3)
  `(chain gl (uniform3f ,location ,value1 ,value2 ,value3)))

(define-ps-macro uniform-matrix-3fv (location transpose-p value)
  `(chain gl (uniform-matrix-3fv ,location ,transpose-p  ,value)))

(define-ps-macro uniform-matrix-4fv (location transpose-p value)
  `(chain gl (uniform-matrix-4fv ,location ,transpose-p  ,value)))

(define-ps-macro get-attrib-location (program name)
  `(chain gl (get-attrib-location ,program ,name)))

(define-ps-macro enable-vertex-attrib-array (index)
  `(chain gl (enable-vertex-attrib-array ,index)))

(define-ps-macro vertex-attrib-pointer (index size type normalized-p stride offset)
  `(chain gl (vertex-attrib-pointer ,index ,size ,(gl-type type) ,normalized-p ,stride ,offset)))

;;; Buffer
(define-ps-macro create-buffer ()
  `(chain gl (create-buffer)))

(define-ps-macro bind-buffer (target buffer)
  `(chain gl (bind-buffer ,(gl-buffer-target target) ,buffer)))

(define-ps-macro buffer-data (target array-buffer-data usage)
  `(chain gl (buffer-data ,(gl-buffer-target target) ,array-buffer-data ,(gl-buffer-usage usage))))

;;; Texture
(define-ps-macro create-texture ()
  `(chain gl (create-texture)))

(define-ps-macro active-texture (target)
  `(chain gl (active-texture ,(gl-texture-unit target))))

(define-ps-macro bind-texture (target texture)
  `(chain gl (bind-texture ,(gl-texture-target target) ,texture)))

(define-ps-macro is-texture (texture)
  `(chain gl (is-texture ,texture)))

(define-ps-macro delete-texture (texture)
  `(chain gl (delete-texture ,texture)))

(define-ps-macro tex-image-2d (target level internal-format format type source)
  `(chain gl (tex-image-2-d ,(gl-texture-target target) ,level ,(gl-format internal-format)
			    ,(gl-format format)
			    ,(gl-type type) ,source)))

(define-ps-macro tex-image-2d-array (target level internal-format width height border format type pixels)
  `(chain gl (tex-image-2-d ,(gl-texture-target target) ,level
			    ,(gl-format internal-format) ,width ,height ,border
			    ,(gl-format format) ,(gl-type type) ,pixels)))

(define-ps-macro tex-parameteri (target param-name param)
  `(chain gl (tex-parameteri ,(gl-texture-target target) ,(gl-texture-parameter param-name)
			     ,(gl-texture-param-name param))))

(define-ps-macro get-tex-parameter (target pname)
  `(chain gl (get-tex-parameter ,(gl-texture-target target) ,(gl-texture-parameter pname))))

(define-ps-macro pixel-storei (pname param)
  `(chain gl (pixel-storei ,(gl-texture-parameter pname) ,param)))

(define-ps-macro generate-mipmap (target)
  `(chain gl (generate-mipmap ,(gl-texture-target target))))



;;;
(defpsmacro install-init-shader ()
  `(setf init-shader
	 (lambda  (vs-src fs-src)
	   (let* ((gl-program (create-program))
		  (vs-shader (create-shader :vertex-shader))
		  (fs-shader (create-shader :fragment-shader)))
	     (shader-source vs-shader vs-src)
	     (shader-source fs-shader fs-src)
	     (compile-shader vs-shader)
	     (unless (get-shader-parameter vs-shader :compile-status)
	       (alert (chain gl (get-shader-info-log vs-shader))))
	     (compile-shader fs-shader)
	     (unless (get-shader-parameter fs-shader :compile-status)
	       (alert (chain gl (get-shader-info-log fs-shader))))
	     (attach-shader gl-program vs-shader)
	     (attach-shader gl-program fs-shader)
	     (link-program gl-program)
	     (unless (get-program-parameter gl-program :link-status)
	       (alert "Could not initialise shaders"))
	     gl-program))))

;;; dummy...
(defun init-shader (vs-src fs-src)
  (declare (ignore vs-src fs-src)))

(define-ps-macro with-gl-canvas ((var canvas-id &optional option) &body body)
  `(progn (setf ,var (chain document (get-element-by-id ,canvas-id)))
	  (setf gl (or (chain ,var (get-context "webgl" ,option ))
			(chain ,var (get-context "experimental-webgl" ,option))))
	  (unless gl
	    (alert "error: your browser does not appear to support webgl"))
	  (install-init-shader)
	  ,@body
	  "undefined"))

