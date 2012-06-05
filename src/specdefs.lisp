(in-package #:cl-cudalib)

;; general
(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by most declaration expressions.")

;(defconstant +arch+ (let* ((process (run-program "/bin/sh" '("-c" "uname -m") :output :stream))
;		      (output (process-output process))
;		      (line (read-line output nil nil))) line)
;  "Calls uname -m to find the system is 32- or 64-bit.")

(defconstant +single-float-size+ 4 "Size of a single float in bytes.")
(defconstant +complex-single-float-size+ 8 "Size of a complex single float in bytes.")
(defconstant +double-float-size+ 8 "Size of a double float in bytes.")
(defconstant +complex-double-float-size+ 16 "Size of a complex double float in bytes.")

;; library specific
;; cudart
;-

;; cuda
;(if (equalp +arch+ "i386") 
    (cffi:defctype cuda-dev-ptr :unsigned-int) ;often used as pointer to pointer ;; for 64-bit, should be unsigned-long
;    (cffi:defctype cuda-dev-ptr :unsigned-long))
(cffi:defctype cuda-ptr-ptr :pointer) ;only in libcudastuff, not really used
(cffi:defctype cuda-error :int) ;; enum, 0 is success
(cffi:defctype size-t :int)
(cffi:defcenum cuda-memcpy-kind
  (:host->host 0)
  (:host->device 1)
  (:device->host 2)
  (:device->device 3))

;; cufft
(defconstant +cufft-r2c+ #x2a "real-to-complex") 
(defconstant +cufft-c2r+ #x2c "complex-to-real")
(defconstant +cufft-c2c+ #x29 "complex-to-complex single-float") 
(defconstant +cufft-z2z+ #x69 "complex-to-complex double-float") 
(defconstant +cufft-forward+ -1)
(defconstant +cufft-inverse+ 1)
(cffi:defctype cufft-handle :unsigned-int)
(cffi:defctype cufft-result :int) ;; enum, 0 is success
(cffi:defctype cufft-type :int) ;; enum, c2c is #x29
;which one of these or both?
(cffi:defctype cufft-complex :float)
;(cffi:defcstruct cufft-complex
;  "Complex number structure."
;  (re :float)
;  (im :float))

;; cublas
(cffi:defctype cublas-status :unsigned-int) ;; 0 is success