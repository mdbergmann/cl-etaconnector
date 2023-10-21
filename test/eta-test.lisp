(defpackage :cl-eta.eta-test
  (:use :cl :fiveam :cl-mock :eta)
  (:nicknames :eta-test)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-test)

(def-suite eta-tests
  :description "ETA tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-tests)

(test init-eta
  (with-mocks ()
    (answer eta-ser-if:open-serial :ok)
    (is (eq :ok (eta-init)))
    (is (= 1 (length (invocations 'eta-ser-if:open-serial))))))

(test init-eta--serial-fail-to-open
  (with-mocks ()
    (answer eta-ser-if:open-serial (error "Can't open!"))

    (handler-case
        (progn 
          (eta-init)
          (error "Should not be here!"))
      (error (e)
        (is (string= (format nil "~a" e) "Can't open!"))))))

(test eta-close
  (with-mocks ()
    (answer eta-ser-if:close-serial :ok)
    (is (eq :ok (eta-close)))))

(test eta-close--err
  (with-mocks ()
    (answer eta-ser-if:close-serial (error "Can't close!"))
    (handler-case
        (progn 
          (eta-close)
          (error "Should not be here!"))
      (error (e)
        (is (string= (format nil "~a" e) "Can't close!"))))))

(test eta-start-record--ok
  (with-mocks ()
    (answer (eta-ser-if:write-serial _port data)
      (assert (equalp data (eta-pkg:new-start-record-pkg))))
    (is (eq :ok (eta-start-record)))
    (is (= 1 (length (invocations 'eta-ser-if:write-serial))))))

(test eta-start-record--err
  (with-mocks ()
    (answer eta-ser-if:write-serial (error "Can't write!"))
    (handler-case
        (progn 
          (eta-start-record)
          (error "Should not be here!"))
      (error (e)
        (is (string= (format nil "~a" e) "Can't write!"))))))

(test eta-stop-record--ok
  (with-mocks ()
    (answer (eta-ser-if:write-serial _port data)
      (assert (equalp data (eta-pkg:new-stop-record-pkg))))
    (is (eq :ok (eta-stop-record)))
    (is (= 1 (length (invocations 'eta-ser-if:write-serial))))))

(test eta-stop-record--err
  (with-mocks ()
    (answer eta-ser-if:write-serial (error "Can't write!"))
    (handler-case
        (progn 
          (eta-stop-record)
          (error "Should not be here!"))
      (error (e)
        (is (string= (format nil "~a" e) "Can't write!"))))))

(test eta-read-monitors--complete--empty-monitors
  (with-mocks ()
    (answer eta-ser-if:read-serial #(123 0 1 2 3 125))
    (answer eta-pkg:extract-pkg (values :eta-monitor '()))

    (is (equalp '() (eta-read-monitors)))
    (is (= 1 (length (invocations 'eta-ser-if:read-serial))))
    (is (= 1 (length (invocations 'eta-pkg:extract-pkg))))))

(test eta-read-monitors--complete--with-monitors
  (with-mocks ()
    (answer eta-ser-if:read-serial #(123 0 1 2 3 125))
    (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))

    (is (equalp '(("FooItem" . 1.1)) (eta-read-monitors)))
    (is (= 1 (length (invocations 'eta-ser-if:read-serial))))
    (is (= 1 (length (invocations 'eta-pkg:extract-pkg))))))

(test eta-read-monitors--incomplete-first-read--with-monitors
  (with-mocks ()
    (let ((read-count 0))
      (answer eta-ser-if:read-serial
        (prog1
          (if (= read-count 0)
              #(123 0 1 2 3)
              #(125))
          (incf read-count)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))

      (is (equalp '(("FooItem" . 1.1)) (eta-read-monitors)))
      (is (= 2 (length (invocations 'eta-ser-if:read-serial))))
      (is (= 1 (length (invocations 'eta-pkg:extract-pkg)))))))

(test eta-read-monitors--package-err
  (with-mocks ()
    (answer eta-ser-if:read-serial #(123 0 1 2 3 125))
    (answer eta-pkg:extract-pkg (values :fail "Extract failure!"))

    (is (eq nil (eta-read-monitors)))))
