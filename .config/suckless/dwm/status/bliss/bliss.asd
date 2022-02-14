;;;; bliss.asd

(asdf:defsystem #:bliss
  :description "Modular statusbar for dwm"
  :author "Alexander Goussas <agoussas@espol.edu.ec"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:bordeaux-threads
               #:cffi
               #:trivial-signal)
  :serial t
  :components ((:file "package")
               (:file "signals")
               (:file "blocks")
               (:file "x")
               (:file "bliss"))
  :build-operation "asdf:program-op"
  :build-pathname "bliss"
  :entry-point "bliss::main")
