(defpackage :cl-eta.eta-helper-test
  (:use :cl :fiveam :cl-mock :cl-eta.helper :miscutils)
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

;; --------------------------------------
;; Solar interface tests
;; --------------------------------------

(test solar-retrieves-power
  (with-mocks ()
    (answer solar-if:read-power (values :ok 101.23 1234.56))

    (is (equalp '(101 1235) (multiple-value-list (solar-read))))
    (is (= (length (invocations 'solar-if:read-power)) 1))))

(test solar-calculates-new-total
  (with-mocks ()
    (answer solar-if:read-power (values :ok 101.23 1234.56))
    (is (equalp '(1235 235)
                (multiple-value-list
                 (calc-solar-total 1000))))))
