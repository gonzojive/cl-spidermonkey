(defpackage org.iodb.cl-spidermonkey.system
  (:use :common-lisp :asdf))
(in-package :org.iodb.cl-spidermonkey-system)

(defsystem cl-spidermonkey
  :description "."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT/X11 License.  See LICENSE file"
  :components ((:module "src"
                        :components
			((:file "package")
                         (:file "spidermonkey-bindings")
                         (:file "port" :depends-on ("package"))))))

#+nil
(defsystem cl-spidermonkey-tests
  :name "cl-spidermonkey-tests"
  :author "Red Daly <reddaly@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "CL-SPIDERMONKEY test suite."
  :components ((:static-file "cl-spidermonkey.asd")
               (:module "test"
                        :components
			((:file "package")
			 (:file "all-tests" :depends-on ("package"))
			 (:parenscript-file "cl-spidermonkey-test" :depends-on ("package")))))
  :depends-on ("stefil" "cl-spidermonkey" "paren-test" "paren-util"))
