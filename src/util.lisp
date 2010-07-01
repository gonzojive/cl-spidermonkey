(in-package :cl-spidermonkey)

(defun evaluate-js-raw (code)
  "Evaluates the Javascript code CODE and returns the jsval result."
  (cffi:with-foreign-strings ((js code)
                              (filename "string.js"))
    (cffi:with-foreign-object (rval 'smlib:jsval)
      (if (not (eql 0
                    (let ((foreign-context (foreign-context *js-context*)))
                      (with-float-traps-masked ()
                        (smlib:js-evaluate-script foreign-context
                                                  (smlib:js-get-global-object foreign-context)
                                                  js 
                                                  (length code)
                                                  filename
                                                  20
                                                  rval)))))

          (cffi:mem-ref rval 'smlib:jsval)
          (error "Error evaluating script.")))))

(defun evaluate-js (code)
  "Evaluates the Javascript code CODE and returns the jsval result."
  (evaluate-js-raw code))

(defun js-value-to-lisp (jsval)
  "Given some rval, returns the lisp equivalent value if there is one,
otherwise returns the original value."
  (let ((explicit (js-value-to-lisp-explicit jsval)))
    (cond
      ((eql :void explicit) nil)
      (t explicit))))

(defun js-string-to-lisp (js-string)
  "Converts a spidermonkey string pointer to a lisp string."

  (let* ((char-array (smlib:js-get-string-chars js-string))
         (len (smlib:js-get-string-length js-string))
         (native-string (make-string len)))
    (loop :for i :from 0 :upto (- len 1)
          :for jschar = (cffi:mem-aref char-array 'smlib:jschar i)
          :do (setf (elt native-string i)
                    ;; TODO ensure proper unicode translation
                    (code-char jschar)))
    native-string))

(defun js-value-to-lisp-explicit (jsval)
  "Given some rval, returns the lisp equivalent value if there is one,
otherwise returns the original value."
  (cond
    ((smlib:jsval-doublep jsval)
     (cffi:with-foreign-object (d :double)
       (if (not (= 0 (smlib:js-value-to-number (foreign-context *js-context*)
                                               jsval
                                               d)))
           (cffi:mem-ref d :double)
           (error "Somehow failed to convert a double jsval (Spidermonkey representation) to Lisp."))))
    ((smlib:jsval-intp jsval)
     (smlib:jsval-to-int jsval))


    ((smlib:jsval-booleanp jsval)
     (if (= smlib:+jsval-true+ jsval)
         t
         nil))
    
    ((smlib:jsval-stringp jsval)
     (js-string-to-lisp (smlib:jsval-to-pointer jsval)))

    ((smlib:jsval-nullp jsval)
     nil)

    ((smlib:jsval-voidp jsval)
     :void)

    ((smlib:jsval-objectp jsval)
     (smlib:jsval-to-pointer jsval))))
