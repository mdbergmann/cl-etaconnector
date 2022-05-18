(defpackage :cl-eta.eta-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta :eta-ser-if)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-test)

(def-suite eta-tests
  :description "ETA tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-tests)

(defparameter *open-serial-called* nil)
(defparameter *write-serial-called* nil)
(defparameter *read-serial-called* 0)

(defclass fake-serial-proxy (eta-ser-if:serial-proxy) ())
(defmethod eta-ser-if:open-serial ((proxy fake-serial-proxy) device)
  (assert proxy)
  (cond
    ((string= "/dev/not-exists" device) (error "Can't open!"))
    (t (setf *open-serial-called* t))))
(defmethod eta-ser-if:write-serial ((proxy fake-serial-proxy) port data)  
  (declare (ignore port data))
  (assert proxy)
  (setf *write-serial-called* 5))
(defmethod eta-ser-if:read-serial ((proxy fake-serial-proxy) port &optional timeout)
  (declare (ignore port timeout))
  ;; we just do a tiny timeout
  (sleep .1)
  (incf *read-serial-called*))

(def-fixture init-destroy ()
  (setf *open-serial-called* nil
        *write-serial-called* nil
        *read-serial-called* 0)
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (change-class eta:*serial-proxy* 'fake-serial-proxy)
         (&body))
    (eta:ensure-shutdown)))
  
(test init-serial
  (with-fixture init-destroy ()
    (is (eq :ok (init-serial "/dev/serial")))
    (is-true *open-serial-called*)))

(test init-serial--fail-to-open
  (with-fixture init-destroy ()
    (let ((init-serial-result (multiple-value-list (init-serial "/dev/not-exists"))))
      (is (eq :fail (car init-serial-result)))
      (is (string= "Can't open!" (second init-serial-result))))))

(test start-record--serial-written
  "Tests that the write function on the serial proxy is called.
This is asynchronous and we don't check a result.
A result will be visible when this function is called on the REPL."
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (= 5 *write-serial-called*))
              1.0))))

(test start-record--serial-written--read-received
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (> *read-serial-called* 1))
              1.0))))

(run! 'init-serial)
(run! 'init-serial--fail-to-open)
(run! 'start-record--serial-written)
(run! 'start-record--serial-written--read-received)


#|
TODO:
OK - test for read continously
- test for call to read handler when data arrived
- test 'start-record' actually sends the proper ETA package
|#
