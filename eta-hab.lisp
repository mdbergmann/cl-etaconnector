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

(defmacro gen-reader-item-tripple (reader-pair
                                   reader-in-pair
                                   qm-pair)
  "Macro that generates 3 items for a reader, a reader-in and a qm item.
The 'reader' (or 'meter') item represents the current value of the currency, water, whatever reader/meter.
The 'reader-in' item represents the last value that was read from the reader/meter and is used to trigger the calculation of the qm item.
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
	   :persistence '(:id influx
                      :frequence :every-change)))
       (destructuring-bind (,item-name . ,item-label)
           ,reader-in-pair
         (defitem ,item-name ,item-label 'float
           :initial-value 0))
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

(defun calc-elec-kmperday (input-item reader-item)
  (let* ((input-state (item:get-item-stateq input-item))
	 (input-value (item:item-state-value input-state))
	 (input-timestamp (item:item-state-timestamp input-state))
	 (reader-state (item:get-item-stateq reader-item))
	 (reader-value (item:item-state-value reader-state))
	 (reader-timestamp (item:item-state-timestamp reader-state)))
    (let* ((diff-ts (- input-timestamp reader-timestamp))
	   (diff-days (/ diff-ts (* 60 60 24))))
      (values 
       (/ (- input-value reader-value) diff-days)
       input-value))))

(defmacro gen-reader-input-rule (reader-item-sym
                                 input-item-sym
                                 per-day-item-sym
                                 label)
  (let ((reader-item (gensym))
        (input-item (gensym))
        (per-day-item (gensym))
        (new-km-per-day (gensym))
        (input-value (gensym)))
    `(defrule ,label
         :when-item-change ,input-item-sym
         :do (lambda (trigger)
               (declare (ignore trigger))
               (let ((,reader-item (get-item ,reader-item-sym))
                     (,input-item (get-item ,input-item-sym))
                     (,per-day-item (get-item ,per-day-item-sym)))
                 (multiple-value-bind (,new-km-per-day ,input-value)
                     (calc-elec-kmperday ,input-item ,reader-item)
                   (item:set-value ,per-day-item ,new-km-per-day)
                   (item:set-value ,reader-item ,input-value)))))))

;; Master reader
;; -------------

(gen-reader-item-tripple '(elec-reader-state . "ElecReaderState")
                         '(elec-reader-state-input . "ElecReaderStateInput")
                         '(elec-kw-per-day . "ElecKWattsPerDay"))

(gen-reader-input-rule 'elec-reader-state
                       'elec-reader-state-input
                       'elec-km-per-day
                       "Calculate elec kw/day from new input")

;; Garden reader
;; -------------

(gen-reader-item-tripple '(elec-garden-reader-state . "ElecGarReaderState")
                         '(elec-garden-reader-state-input . "ElecGarReaderStateInput")
                         '(elec-garden-kw-per-day . "ElecGarKWattsPerDay"))
    
(gen-reader-input-rule 'elec-garden-reader-state
                       'elec-garden-reader-state-input
                       'elec-garden-km-per-day
                       "Calculate elec kw/day (garden) from new input")

;; Altes haus reader
;; -------------

(gen-reader-item-tripple '(elec-oldh-reader-state . "ElecOldReaderState")
                         '(elec-oldh-reader-state-input . "ElecOldReaderStateInput")
                         '(elec-oldh-kw-per-day . "ElecOldKWattsPerDay"))

(gen-reader-input-rule 'elec-oldh-reader-state
                       'elec-oldh-reader-state-input
                       'elec-oldh-km-per-day
                       "Calculate elec kw/day (altes) from new input")

;; -----------------------------
;; Water
;; -----------------------------

(defun calc-water-qmperday (input-item reader-item)
  (let* ((input-state (item:get-item-stateq input-item))
	 (input-value (item:item-state-value input-state))
	 (input-timestamp (item:item-state-timestamp input-state))
	 (reader-state (item:get-item-stateq reader-item))
	 (reader-value (item:item-state-value reader-state))
	 (reader-timestamp (item:item-state-timestamp reader-state)))
    (let* ((diff-ts (- input-timestamp reader-timestamp))
	   (diff-days (/ diff-ts (* 60 60 24))))
      (values 
       (/ (- input-value reader-value) diff-days)
       input-value))))

(defmacro gen-water-qm-rule ((reader-item
                              reader-in-item
                              qm-item
                              label))
  (let ((rule-name (gensym))
        (reader-item-instance (gensym))
        (reader-in-item-instance (gensym))
        (qm-item-instance (gensym))
        (new-qm-per-day (gensym))
        (input-value (gensym)))
    (setf rule-name (format nil "Calculate water (~a) qm/day from new input" label))
    `(defrule ,rule-name
       :when-item-change ,reader-in-item
       :do (lambda (trigger)
	     (declare (ignore trigger))
	     (log:info "Calculate new water (~a) qm/day..." ,label)
	     (let ((,reader-item-instance (get-item ,reader-item))
		   (,reader-in-item-instance (get-item ,reader-in-item))
		   (,qm-item-instance (get-item ,qm-item)))
	       (multiple-value-bind (,new-qm-per-day ,input-value)
		   (calc-water-qmperday
		    ,reader-in-item-instance
		    ,reader-item-instance)
		 (item:set-value ,qm-item-instance ,new-qm-per-day)
		 (item:set-value ,reader-item-instance ,input-value)))))))

;; Main reader
;; -----------

(gen-reader-item-tripple '(water-reader-state . "WaterReaderState")
			'(water-reader-state-input . "WaterReaderStateInput")
			'(water-qm-per-day . "WaterQMPerDay"))

(gen-water-qm-rule ('water-reader-state
		    'water-reader-state-input
		    'water-qm-per-day
		    "master"))

;; Garden reader
;; -------------

(gen-reader-item-tripple '(water-garden-reader-state . "GardenWaterReaderState")
			'(water-garden-reader-state-input . "GardenWaterReaderStateInput")
			'(water-garden-qm-per-day . "GardenGardenQMPerDay"))

(gen-water-qm-rule ('water-garden-reader-state
		    'water-garden-reader-state-input
		    'water-garden-qm-per-day
		    "garden"))

;; Fresh-in reader
;; -----------

(gen-reader-item-tripple '(water-fresh-reader-state . "FreshInWaterReaderState")
			'(water-fresh-reader-state-input . "FreshInWaterReaderStateInput")
			'(water-fresh-qm-per-day . "FreshInWaterQMPerDay"))

(gen-water-qm-rule ('water-fresh-reader-state
		    'water-fresh-reader-state-input
		    'water-fresh-qm-per-day
		    "fresh"))

;; zist-in reader
;; --------------

(gen-reader-item-tripple '(water-zist-reader-state . "ZistInWaterReaderState")
			'(water-zist-reader-state-input . "ZistInWaterReaderStateInput")
			'(water-zist-qm-per-day . "ZistInWaterQMPerDay"))

(gen-water-qm-rule ('water-zist-reader-state
		    'water-zist-reader-state-input
		    'water-zist-qm-per-day
		    "zist"))

;; Garden-Gunda
;; ------------

(gen-reader-item-tripple '(water-alt-garden-reader-state . nil)
			'(water-alt-garden-reader-state-input . nil)
			'(water-alt-garden-qm-per-day . nil))

(gen-water-qm-rule ('water-alt-garden-reader-state
		    'water-alt-garden-reader-state-input
		    'water-alt-garden-qm-per-day
		    "alt-garden"))

;; ---------------------
;; Chips
;; ---------------------

(defun calc-chips-qm3perday (input-item reader-item)
  (let* ((input-state (item:get-item-stateq input-item))
	 (input-value (item:item-state-value input-state))
	 (input-timestamp (item:item-state-timestamp input-state))
	 (reader-state (item:get-item-stateq reader-item))
	 (reader-timestamp (item:item-state-timestamp reader-state)))
    (let* ((diff-ts (- input-timestamp reader-timestamp))
	   (diff-days (/ diff-ts (* 60 60 24))))
      (values 
       (/ input-value diff-days)
       input-value))))

(gen-reader-item-tripple '(chips-reload-volume . "ChipsReloadVolume")
			 '(chips-reload-volume-input . "ChipsReloadVolumeInput")
			 '(chips-qm3-per-day . "ChipsPerDay"))

(defrule "ChipsReloadPerDayRule"
  :when-item-change 'chips-reload-volume-input
  :do (lambda (trigger)
	(declare (ignore trigger))
	(log:info "Calculate new chips qm3/day...")
	(let ((chips-item-instance (get-item 'chips-reload-volume))
	      (input-item-instance (get-item 'chips-reload-volume-input))
	      (qm3-item-instance (get-item 'chips-qm3-per-day)))
	  (multiple-value-bind (new-qm3-per-day input-value)
	      (calc-chips-qm3perday
	       input-item-instance
	       chips-item-instance)
	    (item:set-value qm3-item-instance new-qm3-per-day)
	    (item:set-value chips-item-instance input-value)))))

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
                   ;;(openhab:do-post "SolarPowerMom" value)
		   )
           :call-push-p t)
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
                       (log:debug "Pushing value: ~a" value)
                       ;;(openhab:do-post (second i) value)
                       )
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

(defitem 'fen-pv-total-day "FenPVTotalDay" 'float
  ;; in kWh/day
  :initial-value 0.0
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))

(defitem 'fen-pv-total-last "FenPVTotalLast" 'integer
  ;; in Wh
  :initial-value 0
  :persistence '(:id :default
                 :frequency :every-change
                 :load-on-start t)
  :persistence '(:id :influx
                 :frequency :every-change))  

(defun calc-fen-pv-total-day (current-total last-total last-timestamp)
  (let* ((val-diff (- current-total last-total))
	 (time-diff (- (get-universal-time) last-timestamp))
	 (result (/ val-diff (/ time-diff (* 60 60 24)))))
    result))

(defrule "Calc-Daily-PV-Total" ;in kW
    :when-cron '(:minute 51 :hour 23)
    :do (lambda (trigger)
	  (declare (ignore trigger))
	  (let* ((current-total-value
		   (multiple-value-bind (stat val)
		       (fen-if:read-item "_sum/ProductionActiveEnergy")
		     (case stat
		       (:ok val)
		       (otherwise (error val))))))
	    (multiple-value-bind (last-value last-timestamp)
		(get-item-valueq 'feb-pv-total-last)
	      (item:set-value (get-item 'fen-pv-total-day)
			      (calc-fen-pv-total-day current-total-value
						     last-value
						     last-timestamp))))))

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
