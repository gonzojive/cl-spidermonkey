(in-package :cl-spidermonkey)

(defvar *js-runtime* nil
  "The Spidermonkey runtime instance we are working with--probably
  bound at top level.")

(defvar *js-context* nil
  "The current Javascript context, a Lisp class instance.")

(defvar *js-global-class*
  "The Spidermonkey classing being used as the global object class.")

(defvar *js-global* nil
  "The spidermonkey object being used as the global object.  Probably
  does not need to be a special variable.")

(defvar *error-reporter 'default-error-reporter
  "Function called with 3 args when there is a Javascript error:
1 context
2 message
3 js-error-report")

(defun default-error-reporter (context message report)
  (declare (ignore report context))
  (error "Javascript Error: ~A" message))

(defclass js-context ()
  ((sm-context :initarg :sm-context :reader foreign-context
               :documentation "Foreign instance of the Javascript context.")
   (gc :initarg :gc :initform nil :reader context-gc?))
  (:documentation "Native class for representing a Javascript context,
  mostly for the purpose of garbage collection."))

(defmethod initialize-instance :after ((obj js-context) &rest initargs)
  (declare (ignore initargs))
  (let ((ptr (foreign-context obj)))
    (when (context-gc? obj)
      (trivial-garbage:finalize obj
                                #'(lambda ()
                                    (format t "Destroying context ~A" ptr)
                                    (smlib:js-destroy-context ptr))))))


(cffi:defcallback my-report-error :void ((context smlib:js-context) 
                                         (message :string)
                                         (report smlib:js-error-report))
  (funcall *error-reporter context message report))

(defun create-js-environment (&key (memory (* 1024 1024 48)))
  "Initializes the js runtime if need be, and creates a Javascript
context with a new global object and class.  This will set special
variables *js-runtime* *js-context* *js-global-class* and *js-global*,
so if you don't want those overwritten make sure to rebind them first."

  (let ((runtime (or *js-runtime* (smlib:js-init memory))))
    (setf *js-runtime* runtime)

    (multiple-value-bind (context)
        (create-js-context :runtime runtime)
      (setf *js-context* context)
      context)))

(defun create-js-context (&key (memory (* 1024 1024 16)) (runtime *js-runtime*))
  "Initializes a js-context and returns it."
  ;; MALLOC
  (let* ((context (smlib:js-new-context runtime memory))
         (lisp-context (make-instance 'js-context :sm-context context)))

    (smlib:js-set-options context smlib:+jsoption-var-obj-fix+)
    (smlib:js-set-version context :jsversion-latest)

    (smlib:js-set-error-reporter context (cffi:callback my-report-error))

      ;; MALLOC
    (let* ((global-class (init-global-class
                          (smlib:js-malloc context
                                           (cffi:foreign-type-size 'smlib:js-class))))
           ;; MALLOC
           (global-obj (smlib:js-new-object context global-class
                                            (cffi:null-pointer)
                                            (cffi:null-pointer))))

      (with-float-traps-masked ()
        (smlib:js-init-standard-classes context global-obj))

      lisp-context)))

(defmacro with-js-context ((var) &body body)
  "Creates a new Javascript context and binds it to var, then evaluates body."
  `(let ((*js-context* nil))
     (create-js-environment)
     (unwind-protect 
          (let ((,var *js-context*))
            ,@body)
       (destroy-foreign-context (foreign-context *js-context*)))))
      
(defun init-global-class (class)
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

(defun destroy-foreign-context (context)
  (smlib:js-destroy-context context))

(defun destroy-js-runtime (runtime)
  (smlib:js-finish runtime)
  #+nil
  (smlib:js-shut-down))