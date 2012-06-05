(in-package :cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUDA Driver stuff (libcuda)
;; if not initializing with cuInit, make sure to use a Runtime function first!
;; initialization, must be called with 0
;; none of these are really in use for the moment, I prefer the runtime library
(cffi:defcfun ("cuInit" initcu) cuda-error
  (flags :int))

;; contexts
(cffi:defbitfield cuda-context-flags
  (:sched-spin 1)
  (:sched-yield 2)
  (:blocking-sync 4)
  (:map-host 8)
  (:lmem-resize-to-max 16))

(cffi:defctype cuda-context-handle :pointer)

(cffi:defcfun "cuCtxCreate" cuda-error
  (pctx (:pointer cuda-context-handle))
  (flags cuda-context-flags)
  (device :int))

(cffi:defcfun "cuCtxDestroy" cuda-error
  (ctx cuda-context-handle))

(cffi:defcfun "cuCtxSynchronize" cuda-error)

;; linear allocation
(declaim (inline cu-malloc))
(%define-alien-routine ("cuMemAlloc" cu-malloc) cuda-error
  (pptr cuda-dev-ptr :out)
  (bytes :unsigned-int))

(declaim (inline cu-free))
(cffi:defcfun ("cuMemFree" cu-free) cuda-error
  (ptr cuda-dev-ptr))

;; transfer
(declaim (inline cu-cpyDD))
(cffi:defcfun ("cuMemcpyDtoD" cu-cpyDD) cuda-error
  (dst cuda-dev-ptr)
  (src cuda-dev-ptr)
  (bytes :unsigned-int))

(declaim (inline cu-cpyDH))
(cffi:defcfun ("cuMemcpyDtoH" cu-cpyDH) cuda-error
  (dst cuda-ptr-ptr)
  (src cuda-dev-ptr)
  (bytes :unsigned-int))

(declaim (inline cu-cpyHD))
(cffi:defcfun ("cuMemcpyHtoD" cu-cpyHD) cuda-error
  (dst cuda-dev-ptr)
  (src cuda-ptr-ptr)
  (bytes :unsigned-int))
