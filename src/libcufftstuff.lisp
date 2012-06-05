(in-package :cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUFFT stuff (libcufft)
;; planning
(declaim (inline cufft-plan-2d))
(%define-alien-routine ("cufftPlan2d" cufft-plan-2d) cufft-result
  (plan cufft-handle :out)
  (nx :int)
  (ny :int)
  (type cufft-type))

(defun cu-plan2d-csf (x y)
  (multiple-value-bind (result plan)
      (cufft-plan-2d x y +cufft-c2c+)
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    plan))

(defun cu-plan2d-csft (x y type)
  (multiple-value-bind (result plan)
      (cufft-plan-2d x y type)
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    plan))

(declaim (inline cufft-plan-3d))
(%define-alien-routine ("cufftPlan3d" cufft-plan-3d) cufft-result
  (plan cufft-handle :out)
  (nx :int)
  (ny :int)
  (nz :int)
  (type cufft-type))

(defun cu-plan (x y z)
  (multiple-value-bind (result plan)
      (cufft-plan-3d x y z +cufft-c2c+)
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    plan))

(declaim (inline cufft-destroy))
(cffi:defcfun ("cufftDestroy" cufft-destroy) cufft-result
  "destroys plan"
    (plan cufft-handle))

;; transformations
(declaim (inline cufft-exec-c2c))
(cffi:defcfun ("cufftExecC2C" cufft-exec-c2c) cufft-result
  (plan cufft-handle) ;in 4.1 this should be a *plan, in 3.2 it should be a plan
  (in-data cuda-dev-ptr) ;pointer to single complex data in gpu memory
  (out-data cuda-dev-ptr)
  (direction :int))

(defun cuft2-csf! (in n0 n1 &key (forward t) (normalize t))
  "Performs 2D FFT in n0xn1 array in using the cuFFT library."
  (declare #.*standard-optimize-settings*)
    (declare (type fixnum n0 n1)
	     (type boolean normalize forward)
	     (type (simple-array (complex single-float) 2) in)
	     (values (simple-array (complex single-float) 2) &optional))
    (let* ((count (* n0 n1))
	   (in1 (sb-ext:array-storage-vector in))
	   (device-adr (cu-malloc-csf count))) ;allocate array on device
      (declare (fixnum count )) ;device-adr is an int supposed to become the machine adr
      ;; copy data to device
      (cuda-memcpy device-adr 
		   (get-arr-adr in1)
		   (* count +complex-single-float-size+) 
		   ':host->device)
      ;; plan and execute in-place transform on device
      (let ((plan (cu-plan2d-csf n0 n1)))
	(cufft-exec-c2c plan 
			device-adr
			device-adr
			(if forward 
			    +cufft-forward+
			    +cufft-inverse+))
	(cufft-destroy plan))
      ;; copy result back
      (cuda-memcpy (get-arr-adr in1)
		   device-adr 
		   (* count +complex-single-float-size+)
		   ':device->host)
      ;; deallocate array on device
      (cuda-free device-adr)
      ;; normalize if backward
      (when normalize 
	(let ((1/n (coerce (/ 1.0s0 count) 'single-float)))
	  (dotimes (i count)
	    (setf (aref in1 i) (* 1/n (aref in1 i))))))
      in))

(defun cuft2-csf!-nocopy (in-adr n0 n1 &key (forward t) (normalize t))
  "Performs 2D FFT in n0xn1 array in using the cuFFT library. 
   Array is already in GPU-memory, so only adr is passed as in-adr.
   Normalizes using cuBLAS."
  (declare #.*standard-optimize-settings*)
    (declare (type fixnum n0 n1)
	     (type boolean normalize forward)
	     );(type #.*unsigned-int* in-adr))
    (let* ((count (* n0 n1)))
      (declare (fixnum count))
      ;; plan and execute in-place transform on device
      (let ((plan (cu-plan2d-csf n0 n1)))
	(cufft-exec-c2c plan 
			in-adr
			in-adr
			(if forward 
			    +cufft-forward+
			    +cufft-inverse+))
	(cufft-destroy plan))
      ;; normalize if backward
      (when normalize 
	(let ((1/n (coerce (/ 1.0s0 count) 'single-float)))
	  (cublas-cscal count 1/n in-adr 1)))))

;I can still not get this working in action. The plan seems to be updated by cufft-exec.
(defun cuft2-csf!-nocopy-preplan (in-adr plan n0 n1 &key (forward t) (normalize t))
  "Performs 2D FFT in n0xn1 array in using the cuFFT library. 
   Array is already in GPU-memory, so only adr is passed as in-adr.
   Normalizes using cuBLAS. Accepts a plan for reuse."
  (declare #.*standard-optimize-settings*)
    (declare (type fixnum n0 n1)
	     (type boolean normalize forward)
	     );(type #.*unsigned-int* in-adr))
    (let* ((count (* n0 n1)))
      (declare (fixnum count))
      ;; execute in-place transform on device
      (cufft-exec-c2c plan 
		      in-adr
		      in-adr
		      (if forward 
		          +cufft-forward+
		          +cufft-inverse+))
      ;; normalize if backward
      (when normalize 
	(let ((1/n (coerce (/ 1.0s0 count) 'single-float)))
	  (cublas-cscal count 1/n in-adr 1))))) ;enten bruke denne eller bruke egen med (1/n 0i)*in-adr