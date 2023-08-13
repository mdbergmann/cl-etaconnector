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

(defitem 'zist-sensor-curr "ZistSensorCurrency"
  (binding :initial-delay 5
           :delay 60
           :pull (lambda () (eta-helper:ina-read))
           :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   ;;(openhab:do-post "ZistSensorCurrency" value)
                   )
           :pull-passthrough t)
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t))

