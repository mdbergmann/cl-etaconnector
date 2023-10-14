(defpackage :cl-eta.eta-helper-test
  (:use :cl :fiveam :cl-mock :cl-eta.helper :cl-eta.eta :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-helper-test)

(def-suite eta-helper-tests
  :description "Tests for eta helper interface"
  :in cl-eta.tests:test-suite)

(in-suite eta-helper-tests)

;; --------------------------------------
;; INA219 interface tests
;; --------------------------------------

(test ina-initialization
  (with-mocks ()
    (answer ina219-if:init (values :ok))
    (is-true (ina-init))
    (is (= 1 (length (invocations 'ina219-if:init))))))

(test ina-initialization--err
  (with-mocks ()
    (answer ina219-if:init (values :error))
    (handler-case
        (ina-init)
      (error () (is-true t))
      (:no-error () (is-true nil)))
    (is (= 1 (length (invocations 'ina219-if:init))))))

(test ina-retrieves-currency
  (with-mocks ()
    (answer ina219-if:init (values :ok))
    (ina-init)

    (answer ina219-if:read-currency (values :ok 1.23))
    (is (= (ina-read) 1.23))
    (is (= (length (invocations 'ina219-if:read-currency)) 1))))

(test ina-retrieves-currency--err
  (with-mocks ()
    (answer ina219-if:init (values :ok))
    (ina-init)

    (answer ina219-if:read-currency (values :error "foo"))
    (handler-case
        (ina-read)
      (error () (is-true t))
      (:no-error () (is-true nil)))
    (is (= (length (invocations 'ina219-if:read-currency)) 1))))
