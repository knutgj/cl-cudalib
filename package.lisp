(defpackage #:cl-cudalib
  (:nicknames #:mcc)
  (:use #:cl #:cffi #:sb-sys)
  (:export ;;functions
           ;#:get-arr-adr

           ;;functions - cudart
	   #:cu-malloc-sf
	   #:cu-malloc-csf
	   #:cuda-free
	   #:cuda-memcpy
	   #:cu-memcpy

	   ;;functions - cufft
	   #:cu-plan2d-csf
	   #:cufft-destroy
	   #:cuft2-csf!
           #:cuft2-csf!-nocopy
	   #:cuft2-csf!-nocopy-preplan

	   ;;functions - cublas - single
	   #:cublas-isamax
	   #:cublas-sdot
	   #:cublas-sscal
	   #:cublas-scopy
	   #:cublas-saxpy
	   #:cublas-sasum

           ;;functions - cublas - complex single
           #:cublas-icamax

           ;;functions - cublas - double
	   #:cublas-isamax   

           ;;functions - cublas - complex double
	   #:cublas-izamax

	   ;;functions - mylib
	   #:elementMult
	   #:elementMultd
	   #:elementMultc
	   #:elementMultcd
	   #:cpycomplex
	   #:cpyreal
	   #:setizero
	   #:settoone
	   ))