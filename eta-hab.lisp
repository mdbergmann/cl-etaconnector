(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-eta))

(defpackage :eta-hab
  (:use :cl :cl-hab.hab)
  (:import-from #:cl-hab.persistence
                #:persistence)
  (:import-from #:cl-hab.simple-persistence
                #:make-simple-persistence))
(in-package :eta-hab)

(log:config :warn :sane :this-console :daily "logs/app.log")
(log:config '(cl-hab) :debug)
(log:config '(cl-eta) :debug)

;; configure underlying actor system, timers, cron, etc.
(defconfig)

(defpersistence :default
    (lambda (id)
      (make-simple-persistence id :storage-root-path "default-simple-persistence")))

;; ---------------------
;; Zisterne
;; ---------------------
(defitem 'zist-sensor-curr "ZistSensorCurrency"
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
                 :load-on-start t))

;; ---------------------
;; Solar
;; ---------------------
(defitem 'sol-power-total-day "SolarPowerTotalDay"
  (binding :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   ;;(openhab:do-post "SolarPowerTotalDay" value)
                   )
           :call-push-p t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t))
(defitem 'sol-power-mom "SolarPowerMom"
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
                 :load-on-start t))

(defrule "Calc-Solar-Total"
  :when-cron '(:minute 50 :hour 23)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (eta-helper:calc-solar-total
         (gethash 'sol-power-total-day *items*))))
