(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :py4cl)
  (load #P"src/ina219-if.lisp") ; This helps with recompiling ina219
  (ql:quickload :cl-eta))

(defpackage :eta-hab
  (:use :cl :cl-hab.hab)
  (:import-from #:cl-hab.persistence
                #:persistence)
  (:import-from #:cl-hab.simple-persistence
                #:make-simple-persistence)
  (:import-from #:cl-hab.influx-persistence
                #:make-influx-persistence))
(in-package :eta-hab)

(log:config :warn :sane :this-console :daily "logs/app.log")
(log:config '(cl-hab) :info)
(log:config '(cl-eta) :info)

;; configure underlying actor system, timers, cron, etc.
(defconfig)

(defpersistence :default
    (lambda (id)
      (make-simple-persistence id :storage-root-path #P"~/eta-simple-persistence")))
(defpersistence :influx
    (lambda (id)
      (make-influx-persistence
       id
       :base-url "http://picellar:8086"
       :token "A005mInE0uPMoW6l-kHmsxX1l8XC14Uw0UyAjV20GDq7qev0M1-kaGy77M7JH7wsIrc3-rTm1hRoHZ735Q4tHw=="
       :org "mabe"
       :bucket "hab")))

;; ---------------------
;; Zisterne
;; ---------------------
(defitem 'zist-sensor-curr "ZistSensorCurrency" 'float
  (binding :initial-delay 5
           :delay (* 60 10)
           :pull (lambda () (eta-helper:ina-read))
           :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   (openhab:do-post "ZistSensorCurrency" value))
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

;; ---------------------
;; Solar
;; ---------------------
(defitem 'sol-power-total-last "SolarPowerTotalLast" 'integer
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t))
(defitem 'sol-power-total-day "SolarPowerTotalDay" 'integer
  :initial-value 0
  (binding :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   (openhab:do-post "SolarPowerTotalDay" value))
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))
(defitem 'sol-power-mom "SolarPowerMom" 'integer
  (binding :initial-delay 5
           :delay 30
           :pull (lambda () (eta-helper:solar-read))
           :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   (openhab:do-post "SolarPowerMom" value))
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

(defun calc-daily-solar-total ()
  (log:info "Trigger calc daily solar total")
  (let ((total-day-item (get-item 'sol-power-total-day))
        (total-last-item (get-item 'sol-power-total-last)))
    (future:fcompleted
        (item:get-value total-last-item)
        (value)
      (multiple-value-bind (total daily)
          (eta-helper:calc-solar-total value)
        (item:set-value total-day-item daily)
        (item:set-value total-last-item total)))))

(defrule "Calc-Daily-Solar-Total"
  :when-cron '(:minute 50 :hour 23)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (calc-daily-solar-total)))
        
;; ---------------------
;; Eta
;; ---------------------

(defvar *eta-raw-items*
  '((eta-op-hours "HeatingETAOperatingHours" integer)
    (eta-ign-count "HeatingETAIgnitionCount" integer)
    (eta-temp-abgas "EtaAbgas" float)
    (eta-temp-aussen "EtaTempAussen" float)
    (eta-temp-boiler "EtaBoiler" float)
    (eta-temp-boiler-unten "EtaBoilerUnten" float)
    (eta-temp-boiler-untsolar "EtaBoilerUntSolar" float)
    (eta-temp-kessel "EtaKessel" float)
    (eta-temp-kessel-rueck "EtaKesselRuecklauf" float)
    (eta-temp-kollektor "EtaKollektor" float)
    (eta-temp-puffer-oben "EtaPufferOben" float)
    (eta-temp-puffer-unten "EtaPufferUnten" float)
    (eta-temp-vorlaufmk0 "EtaVorlaufMK0" float)))

(dolist (i *eta-raw-items*)
  (defitem (first i) (second i) (third i)
    (binding :push (lambda (value)
                     (log:debug "Pushing value: ~a" value)
                     (openhab:do-post (second i) value))
             :call-push-p t)
    :persistence '(:id :default
                   :frequency :every-change
                   :load-on-start t)
    :persistence '(:id :influx
                   :frequency :every-change)))

(defitem 'eta-op-hours-day-weekly "HeatingETAOpHoursPerDay" 'integer
  (binding :push (lambda (value)
		   (log:debug "Pushing (HeatingETAOpHoursPerDay) value: ~a" value)
		   (openhab:do-post "HeatingETAOpHoursPerDay" value))
	   :call-push-p t)
  :persistence '(:id :default
		 :frequency :every-change
		 :load-on-start t)
  :persistence '(:id :influx
		 :frequency :every-change))

(defrule "Init externals"
  :when-cron '(:boot-only t)  ; beware all other cron keys are at :every
  :do (lambda (trigger)
        (declare (ignore trigger))
        (eta-helper:ina-init)
        (eta-helper:eta-init)
        (eta-helper:eta-start-record)))

(defun apply-monitors (monitors apply-fun)
  "Applies the given MONITORS to the items by setting the monitor value."
  (dolist (m monitors)
    (let* ((monitor-name (car m))
           (monitor-value (cdr m))
           (item-id (find monitor-name *eta-raw-items* :key #'second :test #'equal))
           (item (get-item (car item-id))))
      (log:info "Monitor: ~a, value: ~a" monitor-name monitor-value)
      (log:info "Item: ~a, value: ~a" item-id item)
      (when item
        (funcall apply-fun item monitor-value)))))

(defrule "Read-ETA-serial"
  :when-cron '(:minute :every)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (let ((monitors (eta-helper:eta-read-monitors)))
          (apply-monitors monitors
                          (lambda (item value)
                            (item:set-value item value))))))

(defun calc-daily-eta-op-hours-weekly ()
  (log:info "Trigger calc daily eta op hours")
  (let ((item (get-item 'eta-op-hours-day-weekly))
        (eta-op-hours-item (get-item 'eta-op-hours))
	(influx-persp (get-persistence :influx)))
    (future:fcompleted
        (item:get-value eta-op-hours-item)
        (current-hours)
      (future:fcompleted
	  (persp:fetch influx-persp
		       eta-op-hours-item
		       (persp:make-relative-range :days 7))
	  (values)
	(let* ((last (persp:persisted-item-value (first values)))
	       (diff (- current-hours last))
	       (avg (round (/ diff 7))))
	  (log:info "Current-hours: ~a, last week: ~a, diff: ~a, avg: ~a" current-hours last diff avg)
	  (item:set-value item avg)
	  )))))
      
(defrule "Calculate Daily OP hours - weekly"
  :when-cron '(:minute 55 :hour 23 :day-of-week 6)
  :do (lambda (trigger)
	(declare (ignore trigger))
	(calc-daily-eta-op-hours-weekly)))

;; todo: rule for calculating averages for ignition and operating hours

#|

|#
