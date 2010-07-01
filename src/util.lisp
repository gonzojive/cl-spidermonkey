(in-package :cl-spidermonkey)


(defun evaluate-js (code)
  "Evaluates the Javascript code CODE and returns the jsval result."
  (cffi:with-foreign-strings ((js code)
                              (filename "string.js"))
    (cffi:with-foreign-object (rval 'smlib:jsval)
      (if (not (eql 0
                    (with-float-traps-masked ()
                      (smlib:js-evaluate-script *js-context* *global*
                                                js 
                                                (length code)
                                                filename
                                                20
                                                rval))))

          (cffi:mem-ref rval 'smlib:jsval)
          (error "Error evaluating script.")))))

(defun js-value-to-lisp (rval)
  "Given some rval, returns the lisp equivalent value if there is one,
otherwise returns the original value."
  (cffi:with-foreign-object (d :double)
          (smlib:js-value-to-number *js-context*
                                    (cffi:mem-ref rval 'smlib:jsval)
                                    d)

          (cffi:mem-ref d :double))