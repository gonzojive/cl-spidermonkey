(in-package :cl-spidermonkey)

(cffi:defcallback my-report-error :void ((context smlib:js-context) 
                                         (message :string)
                                         (report smlib:js-error-report))
  (format t "My report error: ~A" message))

#+nil
(cffi:load-foreign-library "libjs.so" :search-path "/git/cl-spidermonkey/lib/")

(defparameter *rt* nil)
(defparameter *js-context* nil)
(defparameter *js-global-class* nil)
(defparameter *global* nil)

(defun demo()
  (defparameter *rt* (smlib:js-init (* 1024 1024 8)))
  (defparameter *js-context* (smlib:js-new-context *rt* (* 1024 1024 8)))
  
  (smlib:js-set-options *js-context* smlib:+jsoption-var-obj-fix+)
  (smlib:js-set-version *js-context* :jsversion-latest)


  (smlib:js-set-error-reporter *js-context* (cffi:callback my-report-error))

  (defparameter *js-global-class* (smlib:js-malloc *js-context*
                                                   (cffi:foreign-type-size 'smlib:js-class)))
  
  (make-js-class *js-global-class*)
  
  (defparameter *global*
    (smlib:js-new-object *js-context* *js-global-class*
                         (cffi:null-pointer)
                         (cffi:null-pointer)))

  (with-float-traps-masked ()
    (smlib:js-init-standard-classes *js-context* *global*))

  (when nil
    (smlib:js-destroy-context *js-context*)
    (smlib:js-finish *rt*)
    #+nil
    (smlib:js-shut-down)))

(defun evaluate-js-arith (&optional (code "5 + 5"))
  (cffi:with-foreign-strings ((js code)
                              (filename "string.js"))
    (cffi:with-foreign-object (rval 'smlib:jsval)
      (when (not (eql 0
                      (smlib:js-evaluate-script *js-context* *global*
                                                js 
                                                (length code)
                                                filename
                                                20
                                                rval)))
        (cffi:with-foreign-object (d :double)
          (smlib:js-value-to-number *js-context*
                                    (cffi:mem-ref rval 'smlib:jsval)
                                    d)

          (cffi:mem-ref d :double))))))



(defun jsval-to-lisp-number (
        (cffi:with-foreign-object (d :double)
          (smlib:js-value-to-number *js-context*
                                    (cffi:mem-ref rval 'smlib:jsval)
                                    d)

          (cffi:mem-ref d :double))

(defun make-js-class (class)
  (cffi:with-foreign-slots ((smlib:name
                             smlib:flags
                             smlib:add-property
                             smlib:del-property
                             smlib:get-property
                             smlib:set-property
                             smlib:enumerate
                             smlib:resolve
                             smlib:convert
                             smlib:finalize
                             smlib:get-object-ops
                             smlib:check-access
                             smlib:call
                             smlib:construct
                             smlib:xdr-object
                             smlib:has-instance
                             smlib:mark
                             smlib:reserve-slots)
                            class
                            smlib:js-class)
    (cffi:with-foreign-string (global-str "global")
      (setf smlib:name global-str
            smlib:flags smlib:+jsclass-global-flags+
            smlib:add-property (cffi:foreign-symbol-pointer "JS_PropertyStub") 
            smlib:del-property (cffi:foreign-symbol-pointer "JS_PropertyStub") 
            smlib:get-property (cffi:foreign-symbol-pointer "JS_PropertyStub") 
            smlib:set-property (cffi:foreign-symbol-pointer "JS_PropertyStub") 
            smlib:enumerate (cffi:foreign-symbol-pointer "JS_EnumerateStub") 
            smlib:resolve (cffi:foreign-symbol-pointer "JS_ResolveStub") 
            smlib:convert (cffi:foreign-symbol-pointer "JS_ConvertStub") 
            smlib:finalize (cffi:foreign-symbol-pointer "JS_FinalizeStub") 
            smlib:get-object-ops (cffi:null-pointer)
            smlib:check-access (cffi:null-pointer)
            smlib:call (cffi:null-pointer)
            smlib:construct (cffi:null-pointer)
            smlib:xdr-object (cffi:null-pointer)
            smlib:has-instance (cffi:null-pointer)
            smlib:mark (cffi:null-pointer)
            smlib:reserve-slots (cffi:null-pointer)))

    class))



