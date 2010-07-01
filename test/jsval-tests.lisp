(in-package :cl-spidermonkey-tests)

(in-suite spidermonkey-tests)

(deftest test-environment-is-sane? ()
  (is t))

(deftest can-setup-environment? ()
  nil)

(deftest can-evaluate? ()
  (sm::with-js-context (context)
    (is context)
    (spidermonkey::evaluate-js-raw "true;")))

(deftest bool-matches-evaluation? ()
  (sm::with-js-context (context)
    (is (eql smlib:+jsval-true+
             (spidermonkey::evaluate-js-raw "true;")))
    (is (eql smlib:+jsval-false+
             (spidermonkey::evaluate-js-raw "false;")))))


(deftest null-matches-evaluation? ()
  (sm::with-js-context (context)
    (is (eql smlib:+jsval-void+
             (spidermonkey::evaluate-js-raw "undefined;")))
    (is (eql smlib:+jsval-null+
             (spidermonkey::evaluate-js-raw "null;")))))

(deftest void-matches-evaluation? ()
  (sm::with-js-context (context)
    (is (eql smlib:+jsval-void+
             (spidermonkey::evaluate-js-raw "undefined;")))

(deftest constant-ints-match-evaluation? ()
  (sm::with-js-context (context)
    (is (eql smlib:+jsval-zero+
             (spidermonkey::evaluate-js-raw "0;")))))

(deftest basic-ints-match-evaluation? ()
  (sm::with-js-context (context)
    (is (eql (smlib:jsval-for-int 10)
             (spidermonkey::evaluate-js-raw "10;")))
    (is (eql (smlib:jsval-for-int 15)
             (spidermonkey::evaluate-js-raw "5 + 10;")))
    (is (eql (smlib:jsval-for-int -1)
             (spidermonkey::evaluate-js-raw "-1;")))))

(deftest test-jsval-predicates ()
  (sm::with-js-context (context)
    (let ((true (spidermonkey::evaluate-js-raw "true;"))
          (zero (spidermonkey::evaluate-js-raw "0;"))
          (d (spidermonkey::evaluate-js-raw "2.5;")))
      (is (and (smlib:jsval-booleanp true)
               (not (smlib:jsval-intp true))
               (not (smlib:jsval-doublep true))
               (not (smlib:jsval-objectp true))
               (not (smlib:jsval-voidp true))
               (not (smlib:jsval-stringp true))))

      (let ((x zero))
        (is (and (not (smlib:jsval-booleanp x))
                 (smlib:jsval-intp x)
                 (not (smlib:jsval-doublep x))
                 (not (smlib:jsval-objectp x))
                 (not (smlib:jsval-voidp x))
                 (not (smlib:jsval-stringp x)))))

      (let ((x d))
        (is (and (not (smlib:jsval-booleanp x))
                 (not (smlib:jsval-intp x))
                 (smlib:jsval-doublep x)
                 (not (smlib:jsval-objectp x))
                 (not (smlib:jsval-voidp x))
                 (not (smlib:jsval-stringp x))))))))
