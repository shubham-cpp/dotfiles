;;;; x.lisp

(in-package #:bliss)

(cffi:define-foreign-library :libx11
  (t  "libX11.so"))

(cffi:load-foreign-library :libx11)

(cffi:defcfun ("XOpenDisplay" open-display) :pointer
  "Returns the display object."
  (dpy-name :pointer))

(cffi:defctype window :ulong)

(cffi:defcfun ("XDefaultRootWindow" root-window) window
  "Returns a window corresponding to the root window."
  (dpy :pointer))

(cffi:defcfun ("XDefaultScreenOfDisplay" default-screen) :pointer
  (dpy :pointer))

(cffi:defcfun ("XStoreName" store-name) :int
  "Sets the provided window's name."
  (dpy :pointer)
  (window window)
  (name :string))

(cffi:defcfun ("XFlush" xflush) :void
  "Flushes the display."
  (dpy :pointer))

(cffi:defcfun ("XCloseDisplay" close-display) :void
  "Closes the display."
  (dpy :pointer))
