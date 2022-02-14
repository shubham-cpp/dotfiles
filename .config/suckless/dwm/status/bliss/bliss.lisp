;;;; bliss.lisp

(in-package #:bliss)

;; User variables
(defvar *blocks* nil)
(defvar *separator* nil)
(defvar *config-reload-signal* nil)

;; Internal variables
(defvar *config* nil "The user's configuration file.")
(defvar *dpy* nil "The X display.")
(defvar *root* nil "The X root window.")
(defvar *lock* (bt:make-lock)
  "Lock used to avoid two threads from calling XStoreName at the same time.")

(defun get-config ()
  (let* ((conf-dir (or (uiop/os:getenv "XDG_CONFIG_HOME") (uiop/os:getenv "HOME")))
         (conf-file (make-pathname :directory `(:absolute ,conf-dir "bliss") :name "config" :type "lisp")))
    (if (uiop:file-exists-p conf-file) conf-file nil)))

(defun stringify (&rest xs)
  (format nil "~{~A~}" xs))

(defun build-bar ()
  (loop
    :for block :in *blocks*
    :collect (format nil "~:[~;~:*~A ~]~A" (icon block) (output block)) :into bar
    :finally (return (format nil (stringify "~{~A ~^" *separator* " ~}") bar))))

(defun set-root (name)
  (bt:with-lock-held (*lock*)
    (store-name *dpy* *root* name)
    (xflush *dpy*)))

(defun refresh-bar ()
  (set-root (build-bar)))

(defun setup-signals ()
  (set-signal-handler 15 (teardown))
  (set-signal-handler (signal-number *config-reload-signal*)
    (load *config*) (clean-threads) (spawn-blocks)))

(defun load-user-config! ()
  (let ((config (get-config)))
    (when (null config)
      (format *error-output* "Could not load configuration file~%")
      (uiop:quit 1))
    (setf *config* config)
    (load *config*)))

(defun verify-blocks! ()
  (unless (boundp '*blocks*)
    (format *error-output* "You need to define your blocks!~%")
    (uiop:quit 1)))

(defun setup-display! ()
  (setf *dpy* (open-display (cffi:null-pointer)))
  (when (cffi:null-pointer-p *dpy*)
    (format *error-output* "Failed to open display~%")
    (uiop:quit 1))
  (setf *root* (root-window *dpy*)))

(defun setup ()
  (load-user-config!)
  (verify-blocks!)
  (setup-display!)
  (setup-signals))

(defun teardown ()
  (clean-threads)
  (when *dpy* (close-display *dpy*))
  (format t "Bye bye!~%")
  (uiop:quit 0))

(defun clean-threads ()
  "Destroys all running threads to cleanup resources.
Whether this function causes bliss to deadlock is implementation
dependent, as a block thread might be holding the LOCK when it is
destroyed."
  (loop
    :for th :in (bt:all-threads)
    :when (not (eq th (bt:current-thread)))
      :do (bt:destroy-thread th)))

(defun spawn-blocks ()
  "Starts running each block in its own thread."
  (loop
    :for block in *blocks*
    :do (bt:make-thread (lambda ()
                          (spawn-block block)))))

(defun start ()
  (spawn-blocks) (loop (sleep 1000)))

(defun main ()
  (handler-bind ((#+sbcl sb-sys:interactive-interrupt
                  #+ccl  ccl:interrupt-signal-condition
                  #+clisp system::simple-interrupt-condition
                  #+ecl ext:interactive-interrupt
                  #+allegro excl:interrupt-signal
                  #'(lambda (c) (teardown))))
    (setup) (start)))
