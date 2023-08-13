(defpackage :cl-eta.helper
  (:use :cl)
  (:nicknames :eta-helper)
  (:export #:ina-read))

(in-package :cl-eta.helper)

(defun ina-read ()
  (log:debug "Reading ina currency...")
  (multiple-value-bind (stat currency)
      (ina219-if:read-currency)
    (log:info "Reading ina currency...done, value: ~a" currency)
    (case stat
      (:ok
       (if (numberp currency)
           currency
           (error "Currency not a number: ~a" currency)))
      (otherwise
       (error "Read of ina not OK, value: ~a" currency)))))
