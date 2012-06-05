(in-package :cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my cuda lib (mylib)
;; functions
(declaim (inline elementMult))
(cffi:defcfun ("elementMult" elementMult) :void
  "Single float element multiplication of A * B into C."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr)
  (c cuda-dev-ptr))

(declaim (inline elementMultd))
(cffi:defcfun ("elementMultd" elementMultd) :void
  "Destructive single float element multiplication of A * B into B."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr))

(declaim (inline elementMultc))
(cffi:defcfun ("elementMultc" elementMultc) :void
  "Complex single float element multiplication of A * B into C."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr)
  (c cuda-dev-ptr))

(declaim (inline elementMultcd))
(cffi:defcfun ("elementMultcd" elementMultcd) :void
  "Destructive complex single float element multiplication of A * B into B."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr))

(declaim (inline cpycomplex))
(cffi:defcfun ("cpycomplex" cpycomplex) :void
  "Copies data from real-valued A to complex-valued B."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr))

(declaim (inline cpyreal))
(cffi:defcfun ("cpyreal" cpyreal) :void
  "Copies the real part of complex-valued A to real-valued B."
  (n :int)
  (a cuda-dev-ptr)
  (b cuda-dev-ptr))

(declaim (inline setizero))
(cffi:defcfun ("setizero" setizero) :void
  "Sets the ith element of vector A to 0."
  (n :int)
  (a cuda-dev-ptr)
  (i :int))

(declaim (inline settoone))
(cffi:defcfun ("settoone" settoone) :void
  "Sets N elements in A to 1."
  (n :int)
  (a cuda-dev-ptr))

;; test
(declaim (inline hello))
(cffi:defcfun ("hello" hello) :int "Should print Hello twice.")