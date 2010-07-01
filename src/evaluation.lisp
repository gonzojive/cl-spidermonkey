(in-package :cl-spidermonkey)

(defmacro with-jsval-gc-protected ((value) &body body)
  "Executes body with VALUE protected from being collected by the
Spidermonkey Javascript engine."
  (once-only ((value `(smlib:jsval-for-object ,value))
              (foreign-context `(foreign-context *js-context*)))
    (with-gensyms (value-pointer)
      `(cffi:with-foreign-object (,value-pointer 'smlib:jsval)
         (setf (cffi:mem-ref ,value-pointer 'smlib:jsval)
               ,value)
         (smlib:js-add-root ,foreign-context ,value-pointer)
         (unwind-protect (progn ,@body)
           (smlib:js-remove-root ,foreign-context ,value-pointer))))))

(defmacro with-foreign-string+length ((foreign-string strlen lisp-string) &body body)
  "FOREIGN-STRING is not evaluated and is bound to a foreign c string
pointer as in CFFI:WITH-FOREIGN-STRING.  STRLEN is also not evaluated
and bound to the length of the c string."
  (once-only (lisp-string)
    `(let ((,strlen (length ,lisp-string)))
       (cffi:with-foreign-string (,foreign-string ,lisp-string)
         ,@body))))

(defmacro with-foreign-object-and-mem-ref ((pointer memref type) &body body)
  "Binds POINTER to a foreign pointer as if by
CFFI:WITH-FOREIGN-OBJECT and binds RVAL-MEMREF to the memref to that
pointer, using SYMBOL-MACRO."
  (once-only (type)
    `(cffi:with-foreign-object (,pointer ,type)
       (symbol-macrolet ((,memref (cffi:mem-ref ,pointer ,type)))
         ,@body))))
  

(defmacro with-compiled-script ((compiled-script-var  script
                                                      &key
                                                      (filename "compiled-script.js")
                                                      (line 1))
                                &body body)
  "Evaluates script-form and compiles the script for execution,
binding the result to compiled-script-var and then  executing body."
  (once-only (script
              (foreign-context `(foreign-context *js-context*)))
    (with-unique-names (foreign-string length new-object foreign-filename)
      `(with-foreign-string+length (,foreign-string ,length ,script)
         (cffi:with-foreign-string (,foreign-filename ,filename)
           (let* ((,compiled-script-var 
                   (smlib:js-compile-script ,foreign-context
                                            (smlib:js-get-global-object ,foreign-context)
                                            ,foreign-string
                                            ,length
                                            ,foreign-filename
                                            ,line))
                  (,new-object (smlib:js-new-script-object ,foreign-context ,compiled-script-var)))
             (with-jsval-gc-protected (,new-object)
               ,@body)))))))



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
  (js-value-to-lisp (evaluate-js-raw code)))

(defun compile-and-evaluate-js (js &key callback)
  (with-compiled-script (compiled js)
    (with-foreign-object-and-mem-ref (rval-pointer rval 'smlib:jsval)
      (if (not (eql 0
                    (smlib:js-execute-script (foreign-context *js-context*)
                                             (smlib:js-get-global-object (foreign-context *js-context*))
                                             compiled
                                             rval-pointer)))
          (let ((lisp-value (js-value-to-lisp  rval)))
            (if callback
                (funcall callback lisp-value)
                lisp-value))
          (error "Error executing script")))))



