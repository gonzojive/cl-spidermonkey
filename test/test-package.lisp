(cl:in-package :cl-user)

(defpackage :cl-spidermonkey-tests
    (:nicknames :spidermonkey-tests)
  (:use :cl :alexandria :anaphora :hu.dwim.stefil :cl-spidermonkey)
  (:export #:spidermonkey-tests))

(in-package :cl-spidermonkey-tests)

(defsuite spidermonkey-tests)


