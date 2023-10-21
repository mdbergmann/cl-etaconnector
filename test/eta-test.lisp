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

(test eta-write
  (with-mocks ()
    (answer eta-ser-if:write-serial 10)
    (is (equalp '(:ok 10) (multiple-value-list
                           (eta-write #(1 2 3 4 5 6 7 8 9 10)))))))

(test eta-write--err
  (with-mocks ()
    (answer eta-ser-if:write-serial (error "Can't write!"))
    (handler-case
        (progn 
          (eta-write #(1 2 3 4 5 6 7 8 9 10))
          (error "Should not be here!"))
      (error (e)
        (is (string= (format nil "~a" e) "Can't write!"))))))

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

;; (test start-record--read-received--call-parser--no-complete
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (answer eta-pkg:collect-data (values nil `#(123 0 1 2 3)))
      
;;       (is (eq :ok (eta-start-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (and (> *read-serial-called* 0)
;;                            (> (length (invocations 'eta-pkg:collect-data)) 0)))
;;                 10.0)))))

;; (test start-record--read-received--call-parser--complete--empty-monitor
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
;;       (answer eta-pkg:extract-pkg (values :eta-monitor '()))

;;       (is (eq :ok (eta-start-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (and (> *read-serial-called* 0)
;;                            (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
;;                 10.0)))))

;; (test start-record--read-received--call-parser--complete--with-monitor
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
;;       (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
;;       (answer (openhab:do-post res data)
;;         (progn
;;           (assert (equal res "FooItem"))
;;           (assert (= data 1.1))
;;           :ok))

;;       (is (eq :ok (eta-start-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (and (> *read-serial-called* 0)
;;                            (= (length (invocations 'eta-pkg:extract-pkg)) 1)
;;                            (= (length (invocations 'openhab:do-post)) 1)))
;;                 10.0)))))

;; (test start-record--complete--with-monitor--build-avg
;;   "We use `get-state' internal API to retrieve the state of the actor in order to check on the avgs."
;;   (with-fixture init-destroy ('(("FooItem" .
;;                                  (("FooItemAvg1" . nil)
;;                                   ("FooItemAvg2" . nil)))
;;                                 ("FooItem2" .
;;                                  (("FooItem2Avg" . nil)))))
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
;;       (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1) ("FooItem2" . 2.2))))
;;       (answer openhab:do-post :ok)

;;       (is-true (null (eta::eta-actor-state-avgs (eta::eta-get-state))))
;;       (is (eq :ok (eta-start-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (> *read-serial-called* 5))
;;                 10.0))
;; ;;      (is (eq :ok (eta-stop-record)))      
;;       (let* ((state (eta::eta-get-state))
;;              (avgs (eta::eta-actor-state-avgs state)))
;;         (is (= (length avgs) 3))
;;         (is (every (lambda (x) (typep x 'eta::eta-avg-record)) avgs))
;;         (flet ((assert-avg (name initial-value current-value initial-time current-time)
;;                  (find-if
;;                   (lambda (x) (and (string= name (eta::eta-avg-record-cadence-name x))
;;                               (= initial-value (eta::eta-avg-record-initial-value x))
;;                               (= current-value (eta::eta-avg-record-current-value x))
;;                               (> (eta::eta-avg-record-initial-time x) initial-time)
;;                               (> (eta::eta-avg-record-current-time x) current-time)))
;;                   avgs)))
;;           (is-true (and (assert-avg "FooItemAvg1" 1.1 1.1 0 0)
;;                         (assert-avg "FooItemAvg2" 1.1 1.1 0 0)
;;                         (assert-avg "FooItem2Avg" 2.2 2.2 0 0))))))))

;; (test start-record--read-received--call-parser--complete--extract-fail
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
;;       (answer eta-pkg:extract-pkg (values :fail "Extract failure!"))
;;       (answer openhab:do-post nil)

;;       (is (eq :ok (eta-start-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (and (> *read-serial-called* 0)
;;                            (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
;;                 10.0))
;;       (is (= (length (invocations 'openhab:do-post)) 0)))))

;; (test stop-record--serial-written
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (is (eq :ok (eta-stop-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
;;                 10.0)))))

;; (test stop-record--stops-read
;;   (with-fixture init-destroy ()
;;     (is (eq :ok (eta-init)))
;;     (with-mocks ()
;;       (is (eq :ok (eta-start-record)))
;;       (is (eq :ok (eta-stop-record)))
;;       (is-true (miscutils:assert-cond
;;                 (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
;;                 10.0))
;;       (sleep 0.5)
;;       (is (< *read-serial-called* 5)))))
