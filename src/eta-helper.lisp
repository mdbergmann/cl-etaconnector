(defpackage :cl-eta.helper
  (:use :cl :fiveam)
  (:nicknames :eta-helper)
  (:export #:ina-read
           #:ina-init
           #:solar-read
           #:calc-solar-total
           #:fen-read-item
           #:eta-init
           #:eta-close
           #:eta-start-record
           #:eta-stop-record
           #:eta-read-monitors
	       #:hs-compute-new-on-off-state
           #:*eta-serial-device*))

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
           (coerce currency 'short-float)
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
       (log:info "Reading solar...done, value: ~a W, total: ~a W/h" ,power ,total)
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
                     (and (numberp power) (numberp total))
    (values (round power) (round total))))

(defun calc-solar-total (old-total old-timestamp)
  (multiple-value-bind (_power total)
      (solar-read)
    (declare (ignore _power))
    (let* ((time-diff (- (get-universal-time) old-timestamp))
	   (days-diff (float (/ time-diff (* 60 60 24))))
	   (new-daily (round (/ (- total old-total) days-diff))))
      (log:info "Solar total: ~a W/h" total)
      (log:info "Solar daily: ~a W/h" new-daily)
      (values total new-daily))))

;; ----------------------------------------
;; fen
;; ----------------------------------------

(defun fen-read-item (rest-path)
  (log:debug "Reading Fenecon item on path: ~a" rest-path)
  (multiple-value-bind (stat value)
      (fen-if:read-item rest-path)
    (case stat
      (:ok (progn
             (log:info "Received value: ~a for rest item: ~a" value rest-path)
             value))
      (otherwise
       (error "Error on retrieving fen rest value, stat: ~a" stat)))))

;; ----------------------------------------
;; eta
;; ----------------------------------------

(defvar *eta-serial-device* "/dev/ttyUSB0")
(defvar *eta-serial-port* nil)
(defvar +eta-new-empty-data+ #())

(defun eta-init ()
  "Initializes serial port.
Returns `:ok' if successful.
Underlying serial port can raise conditions."
  (log:info "Initializing serial port...")
  (setf *eta-serial-port*
        (eta-ser-if:open-serial *eta-serial-device*))
  (log:info "Initializing serial port...done")
  :ok)

(defun eta-close ()
  "Closes serial port.
Returns `:ok' if successful.
Underlying serial port can raise conditions."
  (log:info "Closing serial port...")
  (eta-ser-if:close-serial *eta-serial-port*)
  (setf *eta-serial-port* nil)
  (log:info "Closing serial port...done")
  :ok)

(defun %eta-write (data)
  "Writes data to serial port.
Returns `(values :ok <written-number-of-bytes>)' if successful.
Underlying serial port can raise conditions."
  (values :ok (eta-ser-if:write-serial *eta-serial-port* data)))

(defun eta-start-record ()
  "Starts recording data from serial port."
  (log:info "Starting record...")
  (prog1
      (%eta-write (eta-pkg:new-start-record-pkg))
    (log:info "Starting record...done")))

(defun eta-stop-record ()
  "Stops recording data from serial port."
  (log:info "Stopping record...")
  (prog1
      (%eta-write (eta-pkg:new-stop-record-pkg))
    (log:info "Stopping record...done")))

(defun %process-complete-pkg (pkg-data)
  "Transmits monitor items to openhab.
Returns monitor items, car item name, cdr item value. Or `nil' if failed."
  (multiple-value-bind (pkg-type items)
      (eta-pkg:extract-pkg pkg-data)
    (case pkg-type
      (:fail (progn
               (log:warn "Failed package extraction: ~a" pkg-data)
               nil))
      (:eta-monitor (progn
                      (log:debug "Monitor data: ~a" pkg-data)
                      items))
      (otherwise (progn
                   (log:info "Unknown extract pkg result!")
                   nil)))))

(defun %eta-read-monitors (serial-data)
"This function is recursive, it will call itself until it receives a complete package."
  (let ((read-data
          (eta-ser-if:read-serial *eta-serial-port*)))
    (log:debug "eta read result: ~a" read-data)
    (multiple-value-bind (complete data)
        (eta-pkg:collect-data serial-data read-data)
      (if complete
          (progn
            (log:debug "eta complete data: ~a" data)
            (%process-complete-pkg data))
          (progn
            (log:debug "eta incomplete data: ~a" data)
            (sleep 1)
            (%eta-read-monitors data))))))

(defun eta-read-monitors ()
  "Reads monitor data from serial port.
Returns monitor items, car item name, cdr item value. Or `nil' if failed."
  (log:debug "eta read monitors...")
  (prog1
      (%eta-read-monitors +eta-new-empty-data+)
    (log:info "eta read monitors...done")))

;; ------------------------------------
;; Heizstab
;; ------------------------------------

(defvar *hs-energy* 1500
  "Heizstab Wendel energy")
(defparameter *hs-on-threshold* 600
  "Threshold on top of `*hs-energy*' until switched on.")

(defun hs-compute-new-on-off-state (hs-states avail-energy &optional (hs-overrides nil))
  "Calculates new on/off states for the Heizstäbe.
Since we only know the currently available energy (in Watt) (that is pushed to grid),
we have to substract the amount of energy that is currently consumed by the Heizstab
in order to determine what should be their next state.

Returns alist of Heizstab symbol and new state."
  (let* ((hs-needed-energy (+ *hs-energy* *hs-on-threshold*))
	     (hs-symbols (mapcar #'car hs-states))
	     (hs-switch-states (mapcar #'cdr hs-states))
	     (eff-avail-energy (+ avail-energy
			                  (reduce (lambda (acc item)
					                    (+ acc (if (eq item 'item:true)
						                           *hs-energy*
						                           0)))
				                      hs-switch-states
				                      :initial-value 0))))
    (loop :for hs :in hs-symbols
	      :for hs-symbol := hs
	      :for i := 1 :then (1+ i)
	      :for min-req-energy := (* i hs-needed-energy)
	      :collect (let ((override (assoc hs hs-overrides)))
                     (if override
                         (cons hs (cdr override))
                         (if (>= eff-avail-energy min-req-energy)
                             (cons hs 'item:true)
                             (cons hs 'item:false)))))))

(test hs-compute-new-on-off-state
  "Tests that the functions returns proper values for new on-off states of Heizstab."
  ;; turn on/off from off state
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:true)
		        (heizstab-wd3 . item:true))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               (+ (* *hs-energy* 3) (* *hs-on-threshold* 3)))))
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:true)
		        (heizstab-wd3 . item:false))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               (+ (* *hs-energy* 2) (* *hs-on-threshold* 2)))))
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:false)
		        (heizstab-wd3 . item:false))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               (+ (* *hs-energy* 1) (* *hs-on-threshold* 1)))))
  ;; turn on/off from on state
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:true)
		        (heizstab-wd3 . item:true))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:true)
		         (heizstab-wd1 . item:true)
		         (heizstab-wd3 . item:true))
               (- (+ (* *hs-energy* 3) (* *hs-on-threshold* 3))
                  (* 3 *hs-energy*)))))
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:true)
		        (heizstab-wd3 . item:false))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:true)
		         (heizstab-wd1 . item:true)
		         (heizstab-wd3 . item:true))
               (- (+ (* *hs-energy* 2) (* *hs-on-threshold* 2))
                  (* 3 *hs-energy*)))))
  (is (equalp '((heizstab-wd2 . item:true)
		        (heizstab-wd1 . item:false)
		        (heizstab-wd3 . item:false))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:true)
		         (heizstab-wd1 . item:true)
		         (heizstab-wd3 . item:true))
               (- (+ (* *hs-energy* 1) (* *hs-on-threshold* 1))
                  (* 3 *hs-energy*)))))
  (is (equalp '((heizstab-wd2 . item:false)
		        (heizstab-wd1 . item:false)
		        (heizstab-wd3 . item:false))
	          (hs-compute-new-on-off-state
	           '((heizstab-wd2 . item:true)
		         (heizstab-wd1 . item:true)
		         (heizstab-wd3 . item:true))
               (- (+ (* *hs-energy* 0) (* *hs-on-threshold* 3))
                  (* 3 *hs-energy*))))))

(test hs-overrides
  ;; activate with override
  (is (equalp '((heizstab-wd2 . item:true)
                (heizstab-wd1 . item:false)
                (heizstab-wd3 . item:true))
              (hs-compute-new-on-off-state
               '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               0 ; not enough energy to activate any
               '((heizstab-wd2 . item:true)
                 (heizstab-wd1 . item:false)
                 (heizstab-wd3 . item:true)))))
  (is (equalp '((heizstab-wd2 . item:true)
                (heizstab-wd1 . item:true)
                (heizstab-wd3 . item:false))
              (hs-compute-new-on-off-state
               '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               (+ *hs-energy* *hs-on-threshold*)
               '((heizstab-wd1 . item:true)))))
  ;; deactivate with override
  (is (equalp '((heizstab-wd2 . item:true)
                (heizstab-wd1 . item:false)
                (heizstab-wd3 . item:false))
              (hs-compute-new-on-off-state
               '((heizstab-wd2 . item:false)
		         (heizstab-wd1 . item:false)
		         (heizstab-wd3 . item:false))
               (+ (* *hs-energy* 2) (* *hs-on-threshold* 2))
               '((heizstab-wd1 . item:false))
               ))))
