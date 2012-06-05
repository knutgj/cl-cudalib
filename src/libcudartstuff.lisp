(in-package :cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUDA Runtime stuff (libcudart)
;; allocation
(declaim (inline cuda-malloc))
(%define-alien-routine ("cudaMalloc" cuda-malloc) cuda-error 
       (device-pointer cuda-dev-ptr :out) 
       (size size-t))

(defun cu-malloc-csf (n)
  (declare (fixnum n))
  (multiple-value-bind (result device-ptr)
      (cuda-malloc (* +complex-single-float-size+ n))
    (unless (eq 0 result)
      (error "Cuda-malloc error: ~a" result))
    device-ptr))

(defun cu-malloc-sf (n)
  (declare (fixnum n))
  (multiple-value-bind (result device-ptr)
      (cuda-malloc (* +single-float-size+ n))
    (unless (eq 0 result)
      (error "Cuda-malloc error: ~a" result))
    device-ptr))


(declaim (inline cuda-free))
(cffi:defcfun ("cudaFree" cuda-free) cuda-error
  "Frees the memory allocated to device-pointer."
    (device-pointer cuda-dev-ptr))

;; transfer
(declaim (inline cuda-memcpy))
(cffi:defcfun ("cudaMemcpy" cuda-memcpy) cuda-error
  "Copies data in memory from dst to src."
  (dst cuda-dev-ptr)
  (src cuda-dev-ptr)
  (count size-t)
  (kind cuda-memcpy-kind))

(defgeneric cu-memcpy (dst src count kind)
  (:documentation
   "Generic function to copy date to/fro the GPU."))

(defmethod cu-memcpy ((dst array) src count kind)
  "Calls cuda-memcopy if one of the arguments is an array."
  (declare #.*standard-optimize-settings*)
    (cuda-memcpy (get-arr-adr dst) src count kind))

(defmethod cu-memcpy (dst (src array) count kind)
  "Calls cuda-memcopy if one of the arguments is an array."
  (declare #.*standard-optimize-settings*)
    (cuda-memcpy dst (get-arr-adr src) count kind))

;; error-check and tests
(declaim (inline cuda-lasterror))
(cffi:defcfun ("cudaGetLastError" cuda-lasterror) cuda-error
  "Prints the last signalled error.")

(defun lasterror ()
  (multiple-value-bind (result)
      (cuda-lasterror)
    (unless (eq 0 result)
      (error "ZOMFG! Last cuda error: ~a" result))
    (print "All A-OK. No errors so far.")))

(declaim (inline cuda-getdevice))
(%define-alien-routine ("cudaGetDevice" cuda-getdevice) cuda-error
	 (device cuda-dev-ptr :out))

(defun getdevice ()
  (multiple-value-bind (result device)
      (cuda-getdevice)
    (print (list result device))
    (unless (eq 0 result)
      (error "device error: ~a" result))
    device))

(declaim (inline cuda-getdevicecount))
(%define-alien-routine ("cudaGetDeviceCount" cuda-getdevicecount) cuda-error
	 (count :int :out))

(defun getdevicecount ()
  (multiple-value-bind (result count)
      (cuda-getdevicecount)
    (print (list result count))
    (unless (eq 0 result)
      (error "device error: ~a" result))
    count))






