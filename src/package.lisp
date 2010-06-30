(cl:in-package :cl-user

(defpackage :cl-spidermonkey
    (:use :cl))


How to generate bindings:
;;(verrazano:generate-binding (list :cffi
;;                                           :package-name :spidermonkey-bindings
;;                                           :input-files '("/git/cl-spidermonkey/include/jsapi.h")
;;                                           :package-nicknames '(:sm-bindings)
;;                                           :working-directory #P"/git/cl-spidermonkey/include/"
;;                                           :gccxml-flags "-I. -DXP_UNIX"))