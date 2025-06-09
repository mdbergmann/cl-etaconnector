;;
;; Main Chipi script defining all items and the whole system
;; load via (asdf:load-system :eta-hab) when in the chipi folder.
;;


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
(log:config '(chipi) :info)
(log:config '(chipi-web) :info)
(log:config '(cl-eta) :info)
(log:config '(eta-hab) :info)
(log:config '(knx-conn) :info)
(log:config :sane :this-console :daily "logs/app.log")

;; configure underlying actor system, timers, cron, etc.
(defconfig "eta-hab"
  (api-env:init :apikey-store (apikey-store:make-simple-file-backend)
                :apikey-lifetime (ltd:duration :day 100))
  ;; maybe create additional api-keys with different access-rights
  ;; see (apikey-store:create-apikey)
  (api:start :address "192.168.50.43")

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
(defpersistence :influx-1m
    (lambda (id)
      (make-influx-persistence
       id
       :base-url "http://picellar:8086"
       :token "A005mInE0uPMoW6l-kHmsxX1l8XC14Uw0UyAjV20GDq7qev0M1-kaGy77M7JH7wsIrc3-rTm1hRoHZ735Q4tHw=="
       :org "mabe"
       :bucket "hab-1month")))

(defparameter *default-persp-every-change*
  '(:id :default
    :frequency :every-change
    :load-on-start t))

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

(defun ma-to-cm (ma)
  "Calculates the cm value for a given sensor value in mA."
  (+ (* 13.33 ma) -75))

(defun ma-to-percent (ma)
  "Convert mA to percentage fill level linearly from 7.8mA = 0% to 13.1mA = 100%."
  (+ (* 18.87 ma) -147.19))

(defitemgroup 'zisterne "Zisterne")

(defitem 'zist-sensor-curr "Strom Zisternen-Sensor [mA]" 'float
  :group '(zisterne)
  (binding :initial-delay 5
           :delay (* 60 10) ;; 10 minutes
           :pull (lambda () (eta-helper:ina-read))
           :push (lambda (value)
                   (log:debug "Pushing value: ~a" value)
                   (openhab:do-post "ZistSensorCurrency" value)
		   ;; calculate fillgrade in cm and set on item
		   (set-item-value 'zist-fillgrade-cm
				   (ma-to-cm value))
		   ;; calculate fillgrade in % and set on item
		   (set-item-value 'zist-fillgrade-percent
				   (ma-to-percent value)))
           :call-push-p t)
  :persistence *default-persp-every-change*
  :persistence '(:id :influx-1m
                 :frequency :every-change))

(defitem 'zist-fillgrade-cm "Füllgrad Zisterne [cm]" 'float
  :initial-value 0.0
  :group '(zisterne)
  (binding :push (lambda (value)
		   (log:debug "Pushing ZistFillgrade value: ~a" value)
		   (openhab:do-post "ZistFillgrade" value))
	   :call-push-p t)
  :persistence *default-persp-every-change*)

(defitem 'zist-fillgrade-percent "Füllgrad Zisterne [%]" 'float
  :initial-value 0.0
  :group '(zisterne)
  (binding :push (lambda (value)
		   (log:debug "Pushing ZistFillgradePercent value: ~a" value)
		   (openhab:do-post "ZistFillgradePercent" value))
	   :call-push-p t)
  :persistence *default-persp-every-change*)

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
    ;;(eta-temp-kollektor "EtaKollektor" float)
    (eta-temp-puffer-oben "EtaPufferOben" float)
    (eta-temp-puffer-unten "EtaPufferUnten" float)
    (eta-temp-vorlaufmk0 "EtaVorlaufMK0" float)))

(defitemgroup 'eta "Eta")

(dolist (i *eta-raw-items*)
  (defitem (first i) (second i) (third i)
    (binding :push (lambda (value)
                     (log:debug "Pushing value: ~a" value)
                     (openhab:do-post (second i) value))
             :call-push-p t)
    :group '(eta)
    :persistence *default-persp-every-change*
    :persistence '(:id :influx
                   :frequency :every-5m)))

(defitem 'eta-op-hours-day-weekly "ETA Operation h/d" 'integer
  (binding :push (lambda (value)
                   (log:debug "Pushing (HeatingETAOpHoursPerDay) value: ~a" value)
                   (openhab:do-post "HeatingETAOpHoursPerDay" value))
           :call-push-p t)
  :group '(eta)
  :persistence *default-persp-every-change*
  :persistence '(:id :influx
                 :frequency :every-change))

(defun apply-monitors (monitors apply-fun)
  "Applies the given MONITORS to the items by setting the monitor value."
  (dolist (m monitors)
    (let* ((monitor-name (car m))
           (monitor-value (cdr m))
           (item-id (find monitor-name *eta-raw-items* :key #'second :test #'equal))
           (item (get-item (car item-id)))
	   (proc-m t))
      (log:debug "Monitor: ~a, value: ~a" monitor-name monitor-value)
      (log:debug "Item: ~a, value: ~a" item-id item)
      (when (and (equal "EtaTempAussen" monitor-name)
		 (< monitor-value -50.0)) ;; sensor is flacky sometimes
	(setf proc-m nil))
      (when (and item proc-m)
        (funcall apply-fun item monitor-value)))))

(defrule "Read-ETA-serial"
  :when-cron '(:minute :every)
  :do (lambda (trigger)
        (declare (ignore trigger))
	(tasks:task-start
	 (lambda ()
	   (let ((monitors (eta-helper:eta-read-monitors)))
	     (apply-monitors monitors
			     (lambda (item value)
			       (item:set-value item value))))))))

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

(defitemgroup 'stromzaehler "Stromzähler")

(defmacro gen-reader-item-double (reader-pair
				                  qm-pair
                                  groups)
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
           :group ,groups
	       (binding :push (lambda (,value-1)
                            (when ,item-label
                              (log:debug "Pushing (~a) value: ~a" ,item-label ,value-1)
                              (openhab:do-post ,item-label ,value-1)))
                    :call-push-p t)
	       :persistence *default-persp-every-change*
	       :persistence '(:id :influx
			              :frequence :every-change)))
       (destructuring-bind (,item-name . ,item-label)
           ,qm-pair
         (defitem ,item-name ,item-label 'float
           :initial-value 0.0
           :group ,groups
           (binding :push (lambda (,value-2)
                            (log:debug "Pushing (~a) value: ~a" ,item-label ,value-2)
                            (openhab:do-post ,item-label ,value-2))
                    :call-push-p t)
           :persistence *default-persp-every-change*
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

(defun submit-mainh-reader-value (reader-value)
  (let ((new-qm-per-day
	      (calc-reader-perday
	       (list reader-value
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

(defun submit-carloader-reader-value (reader-value)
  (let ((new-kw-per-day
	      (calc-reader-perday
	       (list reader-value
		         (get-universal-time))
	       (multiple-value-list
	        (get-item-valueq 'elec-carloader-reader-state)))))
    (set-item-value 'elec-carloader-kw-per-day new-kw-per-day)
    (set-item-value 'elec-carloader-reader-state reader-value)))

;; Master reader
;; -------------

(gen-reader-item-double '(elec-reader-state . "ElecReaderState")
			            '(elec-kw-per-day . "ElecKWattsPerDay")
                        '(stromzaehler))

;; Garden reader
;; -------------

(gen-reader-item-double '(elec-garden-reader-state . "ElecGarReaderState")
			            '(elec-garden-kw-per-day . "ElecGarKWattsPerDay")
                        '(stromzaehler))

;; Altes haus reader
;; -------------

(gen-reader-item-double '(elec-oldh-reader-state . "ElecOldReaderState")
			            '(elec-oldh-kw-per-day . "ElecOldKWattsPerDay")
                        '(stromzaehler))

;; Car loader reader
;; -------------

(gen-reader-item-double '(elec-carloader-reader-state . "ElecCarLoaderReaderState")
			            '(elec-carloader-kw-per-day . "ElecCarLoaderKWattsPerDay")
                        '(stromzaehler))

;; -----------------------------
;; Water
;; -----------------------------

(defitemgroup 'wasser "Wasser")

;; Main reader
;; -----------

(gen-reader-item-double '(water-reader-state . "WaterReaderState")
			            '(water-qm-per-day . "WaterQMPerDay")
                        '(wasser))

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
			            '(water-garden-qm-per-day . "GardenGardenQMPerDay")
                        '(wasser))

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
			            '(water-fresh-qm-per-day . "FreshInWaterQMPerDay")
                        '(wasser))

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
			            '(water-zist-qm-per-day . "ZistInWaterQMPerDay")
                        '(wasser))

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
			            '(water-alt-garden-qm-per-day . nil)
                        '(wasser))

(defun submit-alt-water-reader-value (reader-value)
  (let ((new-qm-per-day
	      (calc-reader-perday
	       (list reader-value
		         (get-universal-time))
	       (multiple-value-list
	        (get-item-valueq 'water-alt-garden-reader-state)))))
    (set-item-value 'water-alt-garden-qm-per-day new-qm-per-day)
    (set-item-value 'water-alt-garden-reader-state reader-value)))

;; ---------------------
;; Chips
;; ---------------------

(defitemgroup 'hackschnitzel "Hackschnitzel")

(gen-reader-item-double '(chips-reload-volume . "ChipsReloadVolume")
			            '(chips-qm3-per-day . "ChipsPerDay")
                        '(hackschnitzel))

(defun submit-chips-value (reader-value)
  (let ((new-chips-per-day
	      (calc-reader-perday
	       (list 0.0
		         (get-universal-time))
	       (multiple-value-list
	        (get-item-valueq 'chips-reload-volume)))))
    (set-item-value 'chips-qm3-per-day new-chips-per-day)
    (set-item-value 'chips-reload-volume reader-value)))

;; ---------------------
;; Solar
;; ---------------------

(defitemgroup 'balkonsolar "Solar Balkon")

(defitem 'sol-power-total-last "SolarPowerTotalLast" 'integer
  ;; this item is just a storage for the last transmitted value
  ;; in W/h
  :group '(balkonsolar)
  :persistence *default-persp-every-change*)
(defitem 'sol-power-total-day "SolarPowerTotalDay" 'integer
  ;; in W/h
  :initial-value 0
  :group '(balkonsolar)
  (binding :push (lambda (value)
                   (log:debug "Pushing (SolarPowerTotalDay) value: ~a" value)
                   (openhab:do-post "SolarPowerTotalDay" value))
           :call-push-p t)
  :persistence *default-persp-every-change*
  :persistence '(:id :influx
                 :frequency :every-change))
(defitem 'sol-power-mom "SolarPowerMom" 'integer
  (binding :initial-delay 5
           :delay 30
           :pull (lambda () (eta-helper:solar-read)))
  :group '(balkonsolar)
  :persistence *default-persp-every-change*
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

(defrule "Calc-Daily-Solar-Total"       ; in kW
  :when-cron '(:minute 50 :hour 23)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (calc-daily-solar-total)))

;; ----------------------------
;; Fenecon readers
;; ----------------------------

(defitemgroup 'fen "Fenecon")

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
      :group '(fen)
      (binding :pull (lambda () (eta-helper:fen-read-item rest-path))
               :push (lambda (value)
                       (log:debug "Pushing value: ~a" value))
               :initial-delay 10
               :delay 30
               :call-push-p t)
      :persistence *default-persp-every-change*
      :persistence '(:id :influx
                     :frequency :every-change))))

;; per day
;; ---------

(defmacro gen-item-fen-total-last (item-sym label type initial-value)
  `(defitem ,item-sym ,label ,type
     :initial-value ,initial-value
     :group '(fen)
     :persistence *default-persp-every-change*))

(defmacro gen-item-fen-total-day (item-sym label type initial-value)
  `(defitem ,item-sym ,label ,type
     :initial-value ,initial-value
     :group '(fen)
     :persistence *default-persp-every-change*
     :persistence '(:id :influx
		            :frequency :every-change)))

(gen-item-fen-total-day 'fen-pv-total-day "FenPVTotalDay" 'float 0.0) ;; in kWh/day
(gen-item-fen-total-last 'fen-pv-total-last "FenPVTotalLast" 'integer 0) ;; in Wh

(gen-item-fen-total-day 'fen-consum-total-day "FenConsumTotalDay" 'float 0.0)
(gen-item-fen-total-last 'fen-consum-total-last "FenConsumTotalLast" 'integer 0)

(gen-item-fen-total-day 'fen-grid-total-day "FenGridTotalDay" 'float 0.0)
(gen-item-fen-total-last 'fen-grid-total-last "FenGridTotalLast" 'integer 0)

(gen-item-fen-total-day 'fen-grid-out-total-day "FenGridOutTotalDay" 'float 0.0)
(gen-item-fen-total-last 'fen-grid-out-total-last "FenGridOutTotalLast" 'integer 0)

(defun calc-fen-total-day (current-total last-total last-timestamp)
  (let* ((val-diff (- current-total last-total))
	     (time-diff (- (get-universal-time) last-timestamp))
	     (days-diff (/ time-diff (* 60 60 24)))
	     (result (truncate (/ val-diff days-diff))))
    result))

(defun calc-daily-pv-total ()
  (log:info "")
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
		            'fen-consum-total-day))
    (ignore-errors
     (process-total "_sum/GridBuyActiveEnergy"
		            'fen-grid-total-last
		            'fen-grid-total-day))
    (ignore-errors
     (process-total "_sum/GridSellActiveEnergy"
		            'fen-grid-out-total-last
		            'fen-grid-out-total-day))
    ))

(defrule "Calc-Daily-PV-Total"          ;in kW
  :when-cron '(:minute 51 :hour 23)
  :do (lambda (trigger)
	    (declare (ignore trigger))
	    (log:info "Running Calc-Daily-PV-Total")
	    (calc-daily-pv-total)))

;; ----------------------------
;; KNX items
;; ----------------------------

;; Plugs

(defitemgroup 'steckdosen "Steckdosen")

(defitem 'switch-plug-garden-south-wall
  "Steckdose Garten Süd Wand" 'boolean
  :group '(steckdosen)
  (knx-binding :ga '(:read "3/4/1" :write "3/4/0")
               :dpt "1.001"
               :call-push-p t))


;; Heizstab (Keller)

(defitemgroup 'heizstab-ov "Heizstab Überschreiben")

(defitem 'heizstab-wd1-override "Heizstab Wd1 überschreiben" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)
(defitem 'heizstab-override-wd1-active "Heizstab Wd1 überschreiben aktiv" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)

(defitem 'heizstab-wd2-override "Heizstab Wd2 überschreiben" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)
(defitem 'heizstab-override-wd2-active "Heizstab Wd2 überschreiben aktiv" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)

(defitem 'heizstab-wd3-override "Heizstab Wd3 überschreiben" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)
(defitem 'heizstab-override-wd3-active "Heizstab Wd3 überschreiben aktiv" 'boolean
  :initial-value 'item:false
  :group '(heizstab-ov)
  :persistence *default-persp-every-change*)

(defitemgroup 'heizstab "Heizstab")

(defitem 'heizstab-wd1 "Heizstab Wendel 1" 'boolean
  :group '(heizstab)
  (knx-binding :ga '(:read "3/1/2" :write "3/1/1")
	           :dpt "1.001"
	           :call-push-p t))
(defitem 'heizstab-wd2 "Heizstab Wendel 2" 'boolean
  :group '(heizstab)
  (knx-binding :ga '(:read "3/1/4" :write "3/1/3")
	           :dpt "1.001"
	           :call-push-p t))
(defitem 'heizstab-wd3 "Heizstab Wendel 3" 'boolean
  :group '(heizstab)
  (knx-binding :ga '(:read "3/1/6" :write "3/1/5")
	           :dpt "1.001"
	           :call-push-p t))

(defun apply-new-hs-states ()
  (let* ((hs1 (cons 'heizstab-wd1 (get-item-valueq 'heizstab-wd1)))
	     (hs2 (cons 'heizstab-wd2 (get-item-valueq 'heizstab-wd2)))
	     (hs3 (cons 'heizstab-wd3 (get-item-valueq 'heizstab-wd3)))
	     (hs-states (list hs2 hs1 hs3)) ; hs2 is main
         (hs-override-wd1 (eq 'item:true (get-item-valueq 'heizstab-override-wd1-active)))
         (hs-override-wd2 (eq 'item:true (get-item-valueq 'heizstab-override-wd2-active)))
         (hs-override-wd3 (eq 'item:true (get-item-valueq 'heizstab-override-wd3-active)))
         (hs-overrides (append '()
                               (when hs-override-wd1
                                 (list (cons 'heizstab-wd1 (get-item-valueq 'heizstab-wd1-override))))
                               (when hs-override-wd2
                                 (list (cons 'heizstab-wd2 (get-item-valueq 'heizstab-wd2-override))))
                               (when hs-override-wd3
                                 (list (cons 'heizstab-wd3 (get-item-valueq 'heizstab-wd3-override))))))
         (avail-energy (- (get-item-valueq 'fen-grid-act-power))) ; negative goes to grid
         (new-states
           (eta-helper:hs-compute-new-on-off-state
            hs-states avail-energy hs-overrides)))
	    (log:info "current-states: ~a, avail-energy: ~a, new-states: ~a, overrides: ~a"
		          hs-states avail-energy new-states hs-overrides)
	    (dolist (new-state new-states)
	      (destructuring-bind (hs . state) new-state
	        (set-item-value hs state)))))

(defrule "New on/off state of Heizstab"
  :when-cron '(:minute :every :step-min 15)
  :do (lambda (trigger)
	    (declare (ignore trigger))
	    (log:info "Running new Heizstab states...")
	    (apply-new-hs-states)))

(defitemgroup 'heizstab-energy "Heizstab Energie")

(defmacro gen-heizstab-energy (id label phase)
  (let ((res (gensym)))
    `(defitem ,id ,label 'float
       :initial-value 0.0
       :group '(heizstab-energy)
       (binding :initial-delay 5
                :delay 60
                :pull (lambda ()
                        (let ((,res (multiple-value-list (shelly-pro-3em:read-power))))
                          (if (eq :ok (car ,res))
                              (nth ,phase (cdr ,res))
                              (error "Unable to read shelly pro 3em power.")))))
       :persistence *default-persp-every-change*
       :persistence '(:id :influx-1m
                      :frequency :every-1m))))

(gen-heizstab-energy 'heizstab-wd1-energy "Heizstab Wendel 1 Energie Wh" 0)
(gen-heizstab-energy 'heizstab-wd2-energy "Heizstab Wendel 2 Energie Wh" 1)
(gen-heizstab-energy 'heizstab-wd3-energy "Heizstab Wendel 3 Energie Wh" 2)

;; Temperatures

(defitemgroup 'temperatur "Temperatur")

(defitem 'temp-outside
  "Temperatur aussen" 'float
  :group '(temperatur)
  (knx-binding :ga "3/2/0"
	           :dpt "9.001"
	           :call-push-p t)
  :persistence *default-persp-every-change*
  :persistence '(:id :influx
		         :frequency :every-30m))

;; Room temperatures




;; -----------------------------
;; change to warn logging after initialization
;; -----------------------------

(log:config '(chipi) :warn)
(log:config '(chipi-web) :warn)
(log:config '(cl-eta) :warn)
(log:config '(eta-hab) :warn)
(log:config '(knx-conn) :info)
