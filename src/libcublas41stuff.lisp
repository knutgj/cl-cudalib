(in-package :cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;;NOTE FROM WIKI:
;For maximum compatibility with existing Fortran environments, CUBLAS uses column‐major storage and 1‐based indexing. Since C and C++ use row‐major storage, applications cannot use the native array semantics for two‐dimensional arrays. Instead, macros or inline functions should be defined to implement matrices on top of onedimensional arrays. For Fortran code ported to C in mechanical fashion, one may chose to retain 1‐based indexing to avoid the need to transform loops. In this case, the array index of a matrix element in row i and column j can be computed via the following macro:
;#define IDX2F(i,j,ld) ((((j)-1)*(ld))+((i)-1))
;Here, ld refers to the leading dimension of the matrix as allocated, which in the case of column‐major storage is the number of rows. For natively written C and C++ code, one would most likely chose 0‐based indexing, in which case the indexing macro becomes
;#define IDX2C(i,j,ld) (((j)*(ld))+(i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUBLAS stuff (libcublas)
;; These functions apply to the cuBLAS 4.1 API, for functions supporting
;; the old API, load the 32 version of this file
;; 

;; update work incomplete below this line

;; initialization
(declaim (inline cublas-init))
(cffi:defcfun ("cublasInit" cublas-init) cublas-status
  "initializes cublas")

(defun init ()
  (multiple-value-bind (result)
      (cublas-init)
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    (print "cuBLAS initiated")))

;; functions
;;
;; complex-single ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline cublas-icamax))
(cffi:defcfun ("cublasIcamax" cublas-icamax) :int
  "returns the index of the element of x with the maximum value"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int))

(defun icamax (n x incx)
  (multiple-value-bind (result)
      (cublas-icamax n x incx)
    result))

(declaim (inline cublas-cscal))
(cffi:defcfun ("cublasCscal" cublas-cscal) :void
  "replaces single‐precision vector x with single‐precision alpha * x"
  (n :int)
  (alpha :float) ;this should be of a cuComplex=float2
  (x cuda-dev-ptr)
  (incx :int))

;; single ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline cublas-isamax))
(cffi:defcfun ("cublasIsamax" cublas-isamax) :int
  "returns the index of the element of x with the maximum value"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int))

(defun isamax (n x incx)
  (multiple-value-bind (result)
      (cublas-isamax n x incx)
    result))

(declaim (inline cublas-sdot))
(cffi:defcfun ("cublasSdot" cublas-sdot) :float
  "computes the dot product of two single‐precision vectors"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int)
  (y cuda-dev-ptr)
  (incy :int))

(declaim (inline cublas-sscal))
(cffi:defcfun ("cublasSscal" cublas-sscal) :void
  "replaces single‐precision vector x with single‐precision alpha * x"
  (n :int)
  (alpha :float)
  (x cuda-dev-ptr)
  (incx :int))

(declaim (inline cublas-scopy))
(cffi:defcfun ("cublasScopy" cublas-scopy) :void
  "copies the single‐precision vector x to the single‐precision vector y"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int)
  (y cuda-dev-ptr)
  (incy :int))

(declaim (inline cublas-saxpy))
(cffi:defcfun ("cublasSaxpy" cublas-saxpy) :void
  "multiplies single‐precision vector x by single‐precision scalar alpha and adds the result to single‐precision vector y"
  (n :int)
  (alpha :float)
  (x cuda-dev-ptr)
  (incx :int)
  (y cuda-dev-ptr)
  (incy :int))

(declaim (inline cublas-sasum))
(cffi:defcfun ("cublasSasum" cublas-sasum) :float
  "computes the sum of the absolute values of the elements of single‐ precision vector x"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int))


;; complex-double ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline cublas-izamax))
(cffi:defcfun ("cublasIzamax" cublas-izamax) :int
  "returns the index of the element of x with the maximum value"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int))

(defun izamax (n x incx)
  (multiple-value-bind (result)
      (cublas-izamax n x incx)
    result))

;; double ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline cublas-idamax))
(cffi:defcfun ("cublasIdamax" cublas-idamax) :int
  "returns the index of the element of x with the maximum value"
  (n :int)
  (x cuda-dev-ptr)
  (incx :int))

(defun idamax (n x incx)
  (multiple-value-bind (result)
      (cublas-idamax n x incx)
    result))