(defpackage :cl-eta.helper
  (:use :cl)
  (:nicknames :eta-helper)
  (:export #:ina-read
           #:ina-init
           #:solar-read
           #:calc-solar-total
           #:eta-init
           #:eta-read-sensors))

(in-package :cl-eta.helper)

;; ----------------------------------------
;; zisterne
;; ----------------------------------------

(defun ina-init ()
  (log:debug "Initializing ina...")
  (case (ina219-if:init)
    (:ok
     (progn
       (log:info "Initializing ina...done")
       t))
    (otherwise
     (error "Initializing ina...failed"))))

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

;; ----------------------------------------
;; solar
;; ----------------------------------------

(defmacro %read-solar-power ((stat power total) pred &body body)
  `(progn
     (log:debug "Reading solar...")
     (multiple-value-bind (,stat ,power ,total)
         (solar-if:read-power)
       (setf ,power (abs ,power)
             ,total (abs ,total))
       (log:info "Reading solar...done, value: ~a, total: ~a" ,power ,total)
       (case ,stat
         (:ok
          (if ,pred
              (progn
                ,@body)
              (error "Invalid number!")))
         (otherwise
          (error "Read of solar not OK!"))))))

(defun solar-read ()
  (%read-solar-power (stat power total)
                     (and (numberp power) (> power 0)
                          (numberp total) (> total 0))
    (values (round power) (round total))))

(defun calc-solar-total (old-total)
  (multiple-value-bind (_power total)
      (solar-read)
    (declare (ignore _power))
    (let ((new-daily (- total old-total)))
      (log:info "Solar total: ~a" total)
      (log:info "Solar daily: ~a" new-daily)
      (values total new-daily))))

;; ----------------------------------------
;; eta
;; ----------------------------------------

(defun eta-init ()
  (log:debug "Initializing eta...")
  ;; (eta:start-record)
  (log:info "Initializing eta...done"))

(defun eta-read-sensors ()
  )
