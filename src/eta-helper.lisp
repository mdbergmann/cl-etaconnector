(defpackage :cl-eta.helper
  (:use :cl)
  (:nicknames :eta-helper)
  (:export #:ina-read
           #:ina-init
           #:solar-read
           #:calc-solar-total
           #:eta-init
           #:eta-close
           #:eta-start-record
           #:eta-stop-record
           #:eta-read-monitors
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
