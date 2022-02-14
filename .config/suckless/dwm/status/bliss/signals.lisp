;;;; signals.lisp

(in-package #:bliss)

;; Took this from SO:
;; https://stackoverflow.com/questions/9950680/unix-signal-handling-in-common-lisp
(defmacro set-signal-handler (signo &body body)
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler)))))
