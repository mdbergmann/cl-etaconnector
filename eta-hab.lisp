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
         (get-item 'sol-power-total-day))))

;; ---------------------
;; Eta
;; ---------------------

;; (defitem 'eta-op-hours "HeatingETAOperatingHours"
;;   (binding :push (lambda (value)
;;                    (log:debug "Pushing value: ~a" value)
;;                    ;;(openhab:do-post "HeatingETAOperatingHours" value)
;;                    )
;;            :call-push-p t)
;;   :persistence '(:id :default
;;                  :frequency :every-change
;;                  :load-on-start t))
;; (defitem 'eta-ign-count "HeatingETAIgnitionCount"
;;   (binding :push (lambda (value)
;;                    (log:debug "Pushing value: ~a" value)
;;                    ;;(openhab:do-post "HeatingETAIgnitionCount" value)
;;                    )
;;            :call-push-p t)
;;   :persistence '(:id :default
;;                  :frequency :every-change
;;                  :load-on-start t))

(defitem 'eta-do-read-monitors "ETA Heating read monitors flag"
  (binding :pull (lambda () nil))
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t))

;; use collection item to pull repeatedly (every 20 seconds.) until complete package.
(defitem 'eta-read-pkg "ETA package reader item"
  (binding :initial-delay 5
           :delay 20
           :pull (lambda () )))


(defvar *eta-read-monitors* nil)
(defrule "Read-ETA-serial"
  :when-item-change 'eta-do-read-monitors
  :do (lambda (trigger)
        (case (car trigger)
          (:item (let ((item (cdr trigger)))
                   (future:fcompleted
                       (item:get-value item)
                       (result)
                     (cond
                       ((and result (not *eta-read-monitors*))
                        ;; start reading
                        )
                       ((and (not result) *eta-read-monitors*)
                        ;; stop reading
                        ))
                     (setf *eta-read-monitors* result)
                     (when *eta-read-monitors*
                       ;; continue reading
                       )))))))
