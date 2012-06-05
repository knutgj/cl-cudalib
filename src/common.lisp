(in-package #:cl-cudalib)
(declaim (optimize speed (debug 1) (safety 2)))

;; libraries
(cffi:define-foreign-library libcuda
  (t (:default "libcuda")))
(cffi:define-foreign-library libcudart
  (t (:default "libcudart")))
(cffi:define-foreign-library libcufft
  (t (:default "libcufft")))
(cffi:define-foreign-library libcublas
  (t (:default "libcublas")))
(cffi:define-foreign-library libmylib
  (t (:default "~/Code/libmylib")))

(cffi:use-foreign-library libcuda)
(cffi:use-foreign-library libcudart)
(cffi:use-foreign-library libcufft)
(cffi:use-foreign-library libcublas)
(cffi:use-foreign-library libmylib)

;; function to get the memory address of arrays
;; this should be the only function calling SBCL-
;; specific functions
(declaim (inline get-arr-adr))
(defun get-arr-adr (arr)
  (sb-sys:sap-int 
   (sb-sys:vector-sap arr)))


;Mimics the behaviour of sb-aliens define-alien-routine. Only needed for routines
;with the :out-keyword. All other routines use the standard cffi-hookup.
;Not mine, do not remember the author. If you do not want to use, simply revert to
;using #:sb-alien and #:sb-c-call. I want to minimize dependencies to #:cffi.
(defmacro %define-alien-routine (name result-type &rest arguments)
  (let* ((lisp-args (loop for arg in arguments
			 when (or (= (length arg) 2)
				  (eq (third arg) :in-out))
			 collect arg))
	 (lisp-vars (mapcar 'first lisp-args))
	 (cffi-args (loop 
		       for arg in arguments
		       for type = (if (member (third arg)
					      '(:out :in-out))
				      :pointer 
				      (second arg))
		       collect (list (first arg) type)))

	 ;; c argument names
	 (c-args (loop for arg in arguments
		      collect (gensym (string (first arg)))))
	 ;; 
	 ;; de-refs for pointers
	 (c-pointer-derefs
	  (loop for arg in arguments
	     for c-arg in c-args
	     for type = (second arg)
	     when (member (third arg) '(:out :in-out))
	     collect `(cffi:mem-ref ,c-arg (quote ,type) 0)))
	 ;;
	 (c-name (if (consp name) (first name) name)) ;; name of foreign function
	 (lisp-name (if (listp name) ;; name of the function we build
			(second name)  ;; must be a symbol
			(intern  (string-upcase name))))
	 ;; 
	 (cffi-defcnum-name (intern
			     (string-upcase
			      (concatenate 'string "%defcfun-"
					   (string c-name))))))
    `(progn
       ;;
       (declaim (inline  ,cffi-defcnum-name))
       ;;
       (cffi:defcfun (,c-name ,cffi-defcnum-name) 
	  ,result-type
	 ,@cffi-args)
       ;;
       (defun ,lisp-name (,@lisp-vars)
	 ,(loop
	     with accum = `(values (,cffi-defcnum-name ,@c-args)
				   ,@c-pointer-derefs)
	     for arg in arguments
	     for argname = (first arg)
	     for type = (second arg)
	     for is-pointer =  (member (third arg) '(:out :in-out))
	     for is-in-out-pointer =  (eq (third arg) :in-out)
	     for c-arg in c-args
	     do
	       (cond 
		 ;; not a pointer, so just wrap in LET
		 ((not is-pointer)
		  (setf
		   accum
		   `(let ((,c-arg ,argname))
		     ,accum)))
		 ;; is a pointer, so do 'with-foreign-object' wrapiping
		 (is-pointer
		  (setf 
		   accum
		   `(cffi:with-foreign-object (,c-arg (quote ,type))
		      ,(when is-in-out-pointer
			     `(setf (mem-ref ,c-arg (quote ,type) 0) ,argname))
		     ,accum))))
	     finally (return accum))))))
