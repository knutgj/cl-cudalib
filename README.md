cl-cudalib
==========

Cl (sbcl) library to call cuda libraries in lisp (cuda, cufft, cublas)

ASDF-installable, but requires installation of cuda, cudart, cufft, and cublas libraries, as well as personal cuda libraries. This linking is set in src/common.lisp.

Check out my other repo, mycudalib, for an example of how you can add the missing pieces of cuda-code to complement your lisp calls.
