;;;; blocks.lisp

(in-package #:bliss)

(defclass block+ ()
  ((icon
    :reader icon
    :initarg :icon
    :initform nil
    :type string
    :documentation
    "The icon that is printed before this block.")
   (command-string
    :reader command-string
    :initarg :command-string
    :type string
    :documentation
    "A string containing the shell command to run when refreshing this block.")
   (interval
    :reader interval
    :initarg :interval
    :initform 30
    :type integer
    :documentation "How often to refresh the block, in seconds.")
   (signo
    :reader signo
    :initarg :signo
    :type keyword
    :documentation "The signal that triggers this block's refreshment, or nil.")
   (output
    :accessor output
    :initform ""
    :type string
    :documentation "This block's output.")))

(defmethod print-object ((object block+) stream)
  (with-slots (icon command-string interval) object
    (format stream "~A[icon=~S,command=~S,interval=~d]"
            (type-of object) icon command-string interval)))

(defmethod refresh-block ((object block+))
  "Runs this block's command and stores it in its output slot."
  (with-accessors ((cmd command-string) (out output)) object
    (multiple-value-bind (stdout error status) (uiop:run-program cmd :output '(:string :stripped t)
                                                                     :error-output :string
                                                                     :ignore-error-status t)
      (if (zerop status)
          (setf out stdout)
          (format *error-output* "Command ~S exited with status ~D: ~A~%" cmd status error)))))

(defmethod spawn-block ((object block+))
  (if (> 0 (interval object))
      (refresh-block object)
      (loop
        (refresh-block object)
        (refresh-bar)
        (sleep (interval object)))))

(defmacro defblock (name &key icon command (interval 30) signo)
  `(progn
     (defparameter ,name (make-instance 'block+
                                        :icon ,icon
                                        :command-string ,(uiop:escape-shell-command command)
                                        :interval ,interval
                                        :signo ,signo))
     (when ,signo
       (set-signal-handler ,(signal-number signo)
                           (refresh-block ,name) (refresh-bar)))
     (refresh-block ,name)))
