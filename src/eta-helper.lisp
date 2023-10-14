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
  (%read-solar-power (stat power _total)
                     (and (numberp power) (> power 0))
    (round power)))

(defun solar-read-total (old-total)
  (%read-solar-power (stat _power total)
                     (and (numberp total) (> total 0))
    (let* ((new-rounded-total (round total))
           (new-daily (- new-rounded-total old-total)))
      (values new-rounded-total new-daily))))

(defun calc-solar-total (old-total)
  (unless (numberp old-total)
    (setf old-total 0))
  (multiple-value-bind (new-total new-daily)
      (solar-read-total old-total)
    (log:info "Solar total: ~a" new-total)
    (log:info "Solar daily: ~a" new-daily)
    new-total))

;; ----------------------------------------
;; eta
;; ----------------------------------------

(defun eta-init ()
  (log:debug "Initializing eta...")
  (log:info "Initializing eta...done"))

(defun eta-read-sensors ()
  )
