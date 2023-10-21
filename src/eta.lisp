(defpackage :cl-eta.eta
  (:use :cl)
  (:nicknames :eta)
  (:export #:eta-init
           #:eta-close
           #:eta-start-record
           #:eta-stop-record
           #:eta-read-monitors
           #:*eta-serial-device*))

(in-package :cl-eta.eta)

(defvar *eta-serial-device* "/dev/ttyUSB0")
(defvar *eta-serial-port* nil)
(defvar +eta-new-empty-data+ #())

(defun eta-init ()
  "Initializes serial port.
Returns `:ok' if successful.
Underlying serial port can raise conditions."
  (setf *eta-serial-port*
        (eta-ser-if:open-serial *eta-serial-device*))
  :ok)

(defun eta-close ()
  "Closes serial port.
Returns `:ok' if successful.
Underlying serial port can raise conditions."
  (eta-ser-if:close-serial *eta-serial-port*)
  (setf *eta-serial-port* nil)
  :ok)

(defun %eta-write (data)
  "Writes data to serial port.
Returns `(values :ok <written-number-of-bytes>)' if successful.
Underlying serial port can raise conditions."
  (values :ok (eta-ser-if:write-serial *eta-serial-port* data)))

(defun eta-start-record ()
  "Starts recording data from serial port."
  (%eta-write (eta-pkg:new-start-record-pkg)))

(defun eta-stop-record ()
  "Stops recording data from serial port."
  (%eta-write (eta-pkg:new-stop-record-pkg)))

(defvar *eta-serial-data* nil)

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
                      (log:info "Monitor data: ~a" pkg-data)
                      items))
      (otherwise (progn
                   (log:info "Unknown extract pkg result!")
                   nil)))))

(defun eta-read-monitors (&optional (serial-data +eta-new-empty-data+))
  "Reads monitor data from serial port.
Returns monitor items, car item name, cdr item value. Or `nil' if failed.
This function is recursive, it will call itself until it receives a complete package."
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
            (eta-read-monitors data))))))
