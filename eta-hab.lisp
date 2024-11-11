(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :py4cl)
  (load #P"src/ina219-if.lisp") ; This helps with recompiling ina219
  (asdf:load-system :cl-eta)
  (asdf:load-asd "/home/manfred/quicklisp/local-projects/chipi/bindings/knx/binding-knx.asd"
                 :name "binding-knx")
  (asdf:load-system :binding-knx))

(defpackage :eta-hab
  (:use :cl :chipi.hab :chipi.binding.knx)
  (:import-from #:chipi.persistence
                #:persistence)
  (:import-from #:chipi.simple-persistence
                #:make-simple-persistence)
  (:import-from #:chipi.influx-persistence
                #:make-influx-persistence))
(in-package :eta-hab)

(log:config :warn)
(log:config '(chipi) :warn)
(log:config '(cl-eta) :info)
(log:config '(eta-hab) :info)
(log:config '(knx-conn) :info)
(log:config :sane :this-console :daily "logs/app.log")

;; configure underlying actor system, timers, cron, etc.
(defconfig
  (knx-init :gw-host "192.168.50.40")
  )

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

(defrule "Init externals"
  :when-cron '(:boot-only t)  ; beware all other cron keys are at :every
  :do (lambda (trigger)
        (declare (ignore trigger))
	(log:info "Initializing externals...")
	(ignore-errors
	 (eta-helper:ina-init))
	(ignore-errors
	 (eta-helper:eta-init)
	 (eta-helper:eta-start-record))
	(log:info "Initializing externals...done")))

;; ---------------------
;; Zisterne
;; ---------------------
(defitem 'zist-sensor-curr "ZistSensorCurrency" 'float
  (binding :initial-delay 5
           :delay (* 60 10) ;; 10 minutes
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
;; Eta
;; ---------------------

(defvar *eta-raw-items*
  '((eta-op-hours "HeatingETAOperatingHours" integer)
    (eta-ign-count "HeatingETAIgnitionCount" integer)
    (eta-temp-abgas "EtaAbgas" float)
    (eta-temp-aussen "EtaTempAussen" float)
    (eta-temp-boiler "EtaBoiler" float)
    (eta-temp-boiler-unten "EtaBoilerUnten" float)
    ;;(eta-temp-boiler-untsolar "EtaBoilerUntSolar" float)
    (eta-temp-kessel "EtaKessel" float)
    (eta-temp-kessel-rueck "EtaKesselRuecklauf" float)
    ;;(eta-temp-kollektor "EtaKollektor" float)
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
                   :frequency :every-5m)))

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

(defun apply-monitors (monitors apply-fun)
  "Applies the given MONITORS to the items by setting the monitor value."
  (dolist (m monitors)
    (let* ((monitor-name (car m))
           (monitor-value (cdr m))
           (item-id (find monitor-name *eta-raw-items* :key #'second :test #'equal))
           (item (get-item (car item-id))))
      (log:debug "Monitor: ~a, value: ~a" monitor-name monitor-value)
      (log:debug "Item: ~a, value: ~a" item-id item)
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


;; ---------------------
;; Strom
;; ---------------------

(defmacro gen-reader-item-double (reader-pair
				  qm-pair)
  "Macro that generates 3 items for a reader, a reader-in and a qm item.
The 'reader' (or 'meter') item represents the current value of the currency, water, whatever reader/meter.
The 'qm' item represents the calculated value per day (or whatever) from the reader/meter."
  (let ((item-name (gensym))
	(item-label (gensym))
	(value-1 (gensym))
	(value-2 (gensym)))
    `(progn
       (destructuring-bind (,item-name . ,item-label)
	   ,reader-pair
	 (defitem ,item-name ,item-label 'float
	   :initial-value 0.0
	   (binding :push (lambda (,value-1)
                        (when ,item-label
                          (log:debug "Pushing (~a) value: ~a" ,item-label ,value-1)
                          (openhab:do-post ,item-label ,value-1)))
                :call-push-p t)
	   :persistence '(:id :default
                      :frequency :every-change
                      :load-on-start t)
	   :persistence '(:id :influx
			  :frequence :every-change)))
       (destructuring-bind (,item-name . ,item-label)
           ,qm-pair
         (defitem ,item-name ,item-label 'float
           :initial-value 0.0
           (binding :push (lambda (,value-2)
                            (log:debug "Pushing (~a) value: ~a" ,item-label ,value-2)
                            (openhab:do-post ,item-label ,value-2))
                    :call-push-p t)
           :persistence '(:id :default
                          :frequency :every-change
                          :load-on-start t)
           :persistence '(:id :influx
                          :frequency :every-change))
         ))))

(defun diff-days (former-ts later-ts)
  (let* ((uni1 (local-time:timestamp-to-universal former-ts))
	 (uni2 (local-time:timestamp-to-universal later-ts))
	 (diff (- uni2 uni1)))
    (round (/ diff (* 60 60 24)))))    

(defun calc-reader-perday (new-reader-values old-reader-values)
  (destructuring-bind (input-value input-timestamp) new-reader-values
    (destructuring-bind (reader-value reader-timestamp) old-reader-values
      (let* ((diff-ts (- input-timestamp reader-timestamp))
	     (diff-days (/ diff-ts (* 60 60 24))))
	(float (/ (- input-value reader-value) diff-days))))))

(defun submit-mainh-reader-value (reader-value oldh-reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list (- reader-value oldh-reader-value)
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'elec-reader-state)))))
    (set-item-value 'elec-kw-per-day new-qm-per-day)
    (set-item-value 'elec-reader-state reader-value)))

(defun submit-oldh-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'elec-oldh-reader-state)))))
    (set-item-value 'elec-oldh-kw-per-day new-qm-per-day)
    (set-item-value 'elec-oldh-reader-state reader-value)))

(defun submit-garden-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'elec-garden-reader-state)))))
    (set-item-value 'elec-garden-kw-per-day new-qm-per-day)
    (set-item-value 'elec-garden-reader-state reader-value)))

;; Master reader
;; -------------

(gen-reader-item-double '(elec-reader-state . "ElecReaderState")
			'(elec-kw-per-day . "ElecKWattsPerDay"))

;; Garden reader
;; -------------

(gen-reader-item-double '(elec-garden-reader-state . "ElecGarReaderState")
			'(elec-garden-kw-per-day . "ElecGarKWattsPerDay"))

;; Altes haus reader
;; -------------

(gen-reader-item-double '(elec-oldh-reader-state . "ElecOldReaderState")
			'(elec-oldh-kw-per-day . "ElecOldKWattsPerDay"))

;; -----------------------------
;; Water
;; -----------------------------

;; Main reader
;; -----------

(gen-reader-item-double '(water-reader-state . "WaterReaderState")
			'(water-qm-per-day . "WaterQMPerDay"))

(defun submit-main-water-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'water-reader-state)))))
    (set-item-value 'water-qm-per-day new-qm-per-day)
    (set-item-value 'water-reader-state reader-value)))

;; Garden reader
;; -------------

(gen-reader-item-double '(water-garden-reader-state . "GardenWaterReaderState")
			'(water-garden-qm-per-day . "GardenGardenQMPerDay"))

(defun submit-garden-water-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'water-garden-reader-state)))))
    (set-item-value 'water-garden-qm-per-day new-qm-per-day)
    (set-item-value 'water-garden-reader-state reader-value)))

;; Fresh-in reader
;; -----------

(gen-reader-item-double '(water-fresh-reader-state . "FreshInWaterReaderState")
			'(water-fresh-qm-per-day . "FreshInWaterQMPerDay"))

(defun submit-fresh-water-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'water-fresh-reader-state)))))
    (set-item-value 'water-fresh-qm-per-day new-qm-per-day)
    (set-item-value 'water-fresh-reader-state reader-value)))

;; zist-in reader
;; --------------

(gen-reader-item-double '(water-zist-reader-state . "ZistInWaterReaderState")
			'(water-zist-qm-per-day . "ZistInWaterQMPerDay"))

(defun submit-zist-water-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'water-zist-reader-state)))))
    (set-item-value 'water-zist-qm-per-day new-qm-per-day)
    (set-item-value 'water-zist-reader-state reader-value)))

;; Garden-Gunda
;; ------------

(gen-reader-item-double '(water-alt-garden-reader-state . nil)
			'(water-alt-garden-qm-per-day . nil))

(defun submit-alt-water-reader-value (reader-value)
  (let ((new-qm-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'water-alt-reader-state)))))
    (set-item-value 'water-alt-qm-per-day new-qm-per-day)
    (set-item-value 'water-alt-reader-state reader-value)))

;; ---------------------
;; Chips
;; ---------------------

(gen-reader-item-double '(chips-reload-volume . "ChipsReloadVolume")
			'(chips-qm3-per-day . "ChipsPerDay"))

(defun submit-chips-value (reader-value)
  (let ((new-chips-per-day
	  (calc-reader-perday
	   (list reader-value
		 (get-universal-time))
	   (multiple-value-list
	    (get-item-valueq 'chips-reload-volume)))))
    (set-item-value 'chips-qm3-per-day new-chips-per-day)
    (set-item-value 'chips-reload-volume reader-value)))

;; ---------------------
;; Solar
;; ---------------------
(defitem 'sol-power-total-last "SolarPowerTotalLast" 'integer
  ;; this item is just a storage for the last transmitted value
  ;; in W/h
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t))
(defitem 'sol-power-total-day "SolarPowerTotalDay" 'integer
  ;; in W/h
  :initial-value 0
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))
(defitem 'sol-power-mom "SolarPowerMom" 'integer
  (binding :initial-delay 5
           :delay 30
           :pull (lambda () (eta-helper:solar-read)))
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

;; on power shutdown, shelly starts with 0 total
;; manually set total item to 0

(defun calc-daily-solar-total ()
  (log:info "Calculating daily total solar...")
  (let ((total-day-item (get-item 'sol-power-total-day))
        (total-last-item (get-item 'sol-power-total-last)))
    (let* ((total-last-state (item:get-item-stateq total-last-item))
	   (total-value (item:item-state-value total-last-state))
	   (total-timestamp (item:item-state-timestamp total-last-state)))
      (log:info "Have last total value: ~a W/h at timestamp: ~a" total-value total-timestamp)
      (multiple-value-bind (total daily)
          (eta-helper:calc-solar-total total-value total-timestamp)
	(log:info "New daily solar: ~a W/h" daily)
	(if (< daily 0)
	    (log:info "Daily value < 0, not recoding.")
	    (progn
	      (item:set-value total-day-item daily)
	      (item:set-value total-last-item total)))))))

(defrule "Calc-Daily-Solar-Total" ; in kW
  :when-cron '(:minute 50 :hour 23)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (calc-daily-solar-total)))

;; ----------------------------
;; Fenecon readers
;; ----------------------------

;; Momentary
;; ----------

(defparameter *fenecon-items*
  '((fen-bat-load-state "FenBatLoadState" "ess0/Soc" integer)
    (fen-bat-charge-act-power "FenBatChargePower" "ess0/DcDischargePower" integer)
    (fen-pv-gen-act-power "FenPVGenActivePower" "_sum/ProductionActivePower" integer)
    (fen-pv-str1-act-power "FenPVStr1ActualPower" "charger10/ActualPower" integer)
    (fen-pv-str2-act-power "FenPVStr2ActualPower" "charger11/ActualPower" integer)
    (fen-grid-act-power "FenGridActualPower" "meter0/ActivePower" integer)
    (fen-consum-act-power "FenConsumptionActivePower" "_sum/ConsumptionActivePower" integer)
    (fen-kacu-act-power "FenKacuActivePower" "pvInverter0/ActivePower" integer)))

(dolist (i *fenecon-items*)
  (destructuring-bind (item-id item-label rest-path item-val-type) i
    (defitem item-id item-label item-val-type
      :initial-value 0
      (binding :pull (lambda () (eta-helper:fen-read-item rest-path))
               :push (lambda (value)
                       (log:debug "Pushing value: ~a" value))
               :initial-delay 10
               :delay 30
               :call-push-p t)
      :persistence '(:id :default
                     :frequency :every-change
                     :load-on-start t)
      :persistence '(:id :influx
                     :frequency :every-change))))

;; per day
;; ---------

(defmacro gen-item-fen-total-last (item-sym label type initial-value)
  `(defitem ,item-sym ,label ,type
     :initial-value ,initial-value
     :persistence '(:id :default
		    :frequency :every-change
		    :load-on-start t)))

(defmacro gen-item-fen-total-day (item-sym label type initial-value)
  `(defitem ,item-sym ,label ,type
     :initial-value ,initial-value
     :persistence '(:id :default
		    :frequency :every-change
		    :load-on-start t)
     :persistence '(:id :influx
		    :frequency :every-change)))

(gen-item-fen-total-day 'fen-pv-total-day "FenPVTotalDay" 'float 0.0) ;; in kWh/day
(gen-item-fen-total-last 'fen-pv-total-last "FenPVTotalLast" 'integer 0) ;; in Wh

(gen-item-fen-total-day 'fen-consum-total-day "FenConsumTotalDay" 'float 0.0)
(gen-item-fen-total-last 'fen-consum-total-last "FenConsumTotalLast" 'integer 0)

(defun calc-fen-total-day (current-total last-total last-timestamp)
  (let* ((val-diff (- current-total last-total))
	 (time-diff (- (get-universal-time) last-timestamp))
	 (result (truncate (/ val-diff (/ time-diff (* 60 60 24))))))
    result))

(defrule "Calc-Daily-PV-Total" ;in kW
    :when-cron '(:minute 51 :hour 23)
    :do (lambda (trigger)
	  (declare (ignore trigger))
	  (flet ((process-total (fen-rest fen-item-total-last fen-item-total-day)
		   (let* ((current-total-value
			    (multiple-value-bind (stat val)
				(fen-if:read-item fen-rest)
			      (case stat
				(:ok val)
				(otherwise (error val))))))
		     (multiple-value-bind (last-value last-timestamp)
			 (get-item-valueq fen-item-total-last)
		       (set-item-value fen-item-total-day
				       (calc-fen-total-day current-total-value
							   last-value
							   last-timestamp))
		       (set-item-value fen-item-total-last current-total-value)))))
	    (ignore-errors
	     (process-total "_sum/ProductionActiveEnergy"
			    'fen-pv-total-last
			    'fen-pv-total-day))
	    (ignore-errors
	     (process-total "_sum/ConsumptionActiveEnergy"
			    'fen-consum-total-last
			    'fen-consum-total-day)))))

;; ----------------------------
;; KNX items
;; ----------------------------

;; Plugs

(defitem 'switch-plug-garden-south-wall
  "Steckdose Garten Süd Wand" 'boolean
  (knx-binding :ga '(:read "3/4/1" :write "3/4/0")
               :dpt "1.001"
               :call-push-p t))


#|

|#
