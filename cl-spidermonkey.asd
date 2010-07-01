(defpackage :org.iodb.cl-spidermonkey.system
  (:use :common-lisp :asdf))

(in-package :org.iodb.cl-spidermonkey.system)

(defsystem :cl-spidermonkey
  :description "."
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT/X11 License.  See LICENSE file"
  :components ((:module "src"
                        :components
			((:file "package")
                         (:file "spidermonkey-bindings" :depends-on ("package"))
                         (:file "port" :depends-on ("package"))
                         (:file "util" :depends-on ("port" "setup-teardown"))
                         (:file "setup-teardown" :depends-on ("port" "spidermonkey-bindings")))))
  :depends-on ("alexandria" "cffi" "anaphora" "trivial-garbage"))

(defsystem :cl-spidermonkey-tests
  :name "cl-spidermonkey-tests"
  :author "Red Daly <reddaly@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "CL-SPIDERMONKEY test suite."
  :components ((:static-file "cl-spidermonkey.asd")
               (:module "test"
                        :components
			((:file "test-package")
			 (:file "jsval-tests" :depends-on ("test-package")))))
  :depends-on ("cl-spidermonkey" "hu.dwim.stefil"))
