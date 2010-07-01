(cl:in-package :cl-user)

(defpackage :cl-spidermonkey
    (:nicknames :spidermonkey :sm)
    (:use :cl :anaphora :alexandria)
  (:export #:with-js-context
           #:evaluate-js
           #:compile-and-evaluate-js))

(in-package :cl-spidermonkey)

(cffi:load-foreign-library "libjs.so"
                           :search-path (asdf:system-relative-pathname (asdf:find-system :cl-spidermonkey)
                                                                       #P"lib/"))


;;;How to generate bindings:
;;(verrazano:generate-binding (list :cffi
;;                                           :package-name :spidermonkey-bindings
;;                                           :input-files '("/git/cl-spidermonkey/include/jsapi.h")
;;                                           :package-nicknames '(:sm-bindings)
;;                                           :working-directory #P"/git/cl-spidermonkey/include/"
;;                                           :gccxml-flags "-I. -DXP_UNIX"))