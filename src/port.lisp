(in-package :cl-spidermonkey)

(defmacro with-float-traps-masked (ignored &body body)
  (declare (ignore ignored))
  #+sbcl
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-sbcl
  `(progn ,@body))