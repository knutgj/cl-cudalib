(defsystem #:cl-cudalib
    :name "cl-cudalib"
    :version "3.0.0"
    :description "Package for general access to cuda libraries (cufft, cublas, ) from lisp"
    :author "Knut"
    :maintainer "Knut S. Gjerden <knut.gjerden@ntnu.no>"
    :long-description "Works, but should continously be tested. Package developing from cl-cufft. Every new thing that works in cufft should be brought into cl-cufft. Current target is to encompass self-made cuda library and cublas and further support for cufft. Evolved from cl-cufft early 2011."
    :licence "LLGPL"
    :serial t
    :depends-on (:cffi)
    :components
  ((:module "package-init"
    :pathname #P""
    :components ((:file "package")))
   (:module "utilities"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "specdefs")
     (:file "common")
     (:file "libcudastuff")
     (:file "libcudartstuff")
     (:file "libcublas32stuff")
     (:file "libcufftstuff")
     (:file "libmylibstuff")))))
 

;    ((:file "package")
;     (:file "specdefs" :depends-on ("package"))
;     (:file "common" :depends-on ("specdefs"))
;     (:file "cuda" :depends-on ("common"))))
