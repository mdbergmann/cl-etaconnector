(eval-when (:compile-toplevel :load-toplevel :execute)
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
(log:config '(cl-hab) :debug)
(log:config '(cl-eta) :debug)

;; configure underlying actor system, timers, cron, etc.
(defconfig)

(defpersistence :default
    (lambda (id)
      (make-simple-persistence id :storage-root-path "default-simple-persistence")))
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
           :delay 60
           :pull (lambda () (eta-helper:ina-read))
           :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   ;;(openhab:do-post "ZistSensorCurrency" value)
                   )
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

;; ---------------------
;; Solar
;; ---------------------
(defitem 'sol-power-total-day "SolarPowerTotalDay" 'integer
  (binding :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   ;;(openhab:do-post "SolarPowerTotalDay" value)
                   )
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
                   ;;(openhab:do-post "SolarPowerMom" value)
                   )
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

(defrule "Calc-Daily-Solar-Total"
  :when-cron '(:minute 50 :hour 23)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (let ((total-item (get-item 'sol-power-total-day)))
          (future:fcompleted
              (item:get-value total-item)
              (value)
            (multiple-value-bind (_total daily)
                (eta-helper:calc-solar-total value)
              (decare (ignore _total))
              (item:set-value total-item daily))))))

;; ---------------------
;; Eta
;; ---------------------

(defparameter *eta-raw-items*
  (list
   ('eta-op-hours "HeatingETAOperatingHours" 'integer)
   ('eta-ign-count "HeatingETAIgnitionCount" 'integer)
   ('eta-temp-abgas "EtaAbgas" 'float)
   ('eta-temp-aussen "EtaTempAussen" 'float)
   ('eta-temp-boiler "EtaBoiler" 'float)
   ('eta-temp-boiler-unten "EtaBoilerUnten" 'float)
   ('eta-temp-boiler-untsolar "EtaBoilerUntSolar" 'float)
   ('eta-temp-kessel "EtaKessel" 'float)
   ('eta-temp-kessel-rueck "EtaKesselRuecklauf" 'float)
   ('eta-temp-kollektor "EtaKollektor" 'float)
   ('eta-temp-puffer-oben "EtaPufferOben" 'float)
   ('eta-temp-puffer-unten "EtaPufferUnten" 'float)
   ('eta-temp-vorlaufmk0 "EtaVorlaufMK0" 'float)))

(dolist (i *eta-raw-items*)
  (defitem (first i) (second i) (third i)
    (binding :push (lambda (value)
                     (log:debug "Pushing value: ~a" value)
                     ;;(openhab:do-post "HeatingETAOperatingHours" value)
                     )
             :call-push-p t)
    :persistence '(:id :default
                   :frequency :every-change
                   :load-on-start t)
    :persistence '(:id :influx
                   :frequency :every-change)))


(defrule "Init externals"
  :when-cron '(:bootonly t)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (eta-helper:ina-init)
        (eta-helper:eta-init)))

(defrule "Read-ETA-serial"
  :when-cron '() ; this is every minute, the lowest granularity.
  :do (lambda (trigger)
        (declare (ignore trigger))
        
        ))
