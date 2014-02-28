
(defpackage #:websock
  (:use #:cl #:ps)
  (:export #:install-websock
	   #:start-websocket-server
	   #:in-ws-repl))

(in-package #:websock)

#+sbcl (load "id-map.lisp")
#+sbcl (use-package :id-map)

#+ccl (import '(ccl::make-id-map ccl::assign-id-map-id ccl::id-map-free-object))

(defvar *client* nil)
(defvar *id-map* nil)

(defclass message-box ()
  ((result :initform nil :accessor result)
   (errorp :initform nil :accessor errorp)
   (semaphore :initarg :semaphore :reader semaphore)))

(defclass repl-resource (ws:ws-resource)
  ())

(defmethod ws:resource-client-connected ((res repl-resource) client)
  (unless *client*
    (setf *client* client)
    (format t "~&got connection on repl~%"))
  t)

(defmethod ws:resource-client-disconnected ((res repl-resource) client)
  (format t "~&disconnected resouce ~a~%" client)
  (setf *client* nil))

(defmethod ws:resource-received-text ((res repl-resource) client message)
  (declare (ignore client))
  (multiple-value-bind (identifiersi offset)
      (read-from-string message)
    (destructuring-bind (id numberp errorp)
	identifiersi
      (let ((result (subseq message offset))
	    (object (id-map-free-object *id-map* id)))
	(when numberp (setf result (parse-number:parse-number result)))
	(setf (errorp object) errorp)
	(setf (result object) result)
	(bt-sem:signal-semaphore (semaphore object))))))

(defpsmacro install-websock (port)
  `(progn
     (defvar socket (new (-web-socket (lisp (format nil "ws://127.0.0.1:~a/repl" ,port)))))
     (setf (chain socket onopen)
	   (lambda () (chain console (log "openning connect to websocket"))))
     (setf (chain socket onmessage)
	   (lambda (msg)
	     (chain console (log "receive message"))
	     (let* ((json (eval (@ msg data)))
		    (id (@ json id))
		    (result nil)
		    (numberp nil)
		    (errorp "NIL"))
	       (try (progn (setf result (eval (@ json task))
				 numberp (if (string= (typeof result) "number") "T" "NIL")))
		    (:catch (error)
		      (setf result error
			    numberp "NIL"
			    errorp "T"))
		    (:finally (chain socket (send (+ "("  id " " numberp " " errorp ")" result))))))))))

(defun start-websocket-server (port)
  (setf *id-map* (make-id-map))
  (bt:make-thread (lambda () (ws:run-server port)) :name "websocket server")
  (ws:register-global-resource "/repl"
			     (make-instance 'repl-resource)
			     (ws:origin-prefix "http://127.0.0.1" "http://localhost"))
  (bt:make-thread
   (lambda ()
     (ws:run-resource-listener (ws:find-global-resource "/repl")))
   :name "resource listener for /repl"))



(define-condition websocket-repl-error (error)
  ((error-report :initarg :error-report :reader error-report))
  (:report (lambda (condition stream)
	     (format stream "~a" (error-report condition)))))

(defmacro in-ws-repl (&body body)
  `(let* ((object (make-instance 'message-box :semaphore (bt-sem:make-semaphore)))
	  (id (assign-id-map-id *id-map* object)))
     (ws:write-to-client-text
      *client*
      (ps (create :id (lisp id) :task (lisp (ps ,@body)))))
     (bt-sem:wait-on-semaphore (semaphore object))
     (if (errorp object) (error 'websocket-repl-error :error-report (result object))
	 (result object))))










