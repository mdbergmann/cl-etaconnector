(defpackage :cl-eta.ina219-if
  (:use :cl)
  (:nicknames :ina219-if)
  (:export #:init
           #:read-currency
           #:cleanup))

(in-package :cl-eta.ina219-if)

;; INA219 Constants
(defparameter *ina219-address* #x40)  ; Default I2C address
(defparameter *shunt-ohm* 0.1)
(defparameter *max-currency* 0.4)

;; INA219 Register addresses
(defparameter +config-register+ #x00)
(defparameter +shunt-voltage-register+ #x01)
(defparameter +bus-voltage-register+ #x02)
(defparameter +power-register+ #x03)
(defparameter +current-register+ #x04)
(defparameter +calibration-register+ #x05)

;; Configuration values
(defparameter +range-16v+ #x0000)     ; 0-16V range
(defparameter +gain-1-40mv+ #x0000)   ; ±40mV range
(defparameter +bus-adc-12bit+ #x0400) ; 12-bit bus ADC
(defparameter +shunt-adc-12bit+ #x0008) ; 12-bit shunt ADC
(defparameter +mode-continuous+ #x0007) ; Continuous conversion mode

(defvar *i2c-handle* nil)

(defun init ()
  "Initialize the INA219 sensor via I2C using cl-pigpio high-level interface"
  (handler-case
      (progn
        ;; Connect to the pigpio daemon using cl-pigpio
        (cl-pigpio:open-pidev/daemon 
         :host "localhost" :port "8888" :set-default t)
        
        ;; Open I2C connection - takes bus and address as positional args
        (setf *i2c-handle* 
              (cl-pigpio:i2c-open 1 *ina219-address*))
        
        ;; Configure the INA219
        (%configure-ina219)
        
        (values :ok))
    (error (e)
      (values :error (format nil "Failed to initialize INA219: ~A" e)))))

(defun %configure-ina219 ()
  "Configure the INA219 with appropriate settings"
  (let ((config-value (logior +range-16v+ 
                              +gain-1-40mv+ 
                              +bus-adc-12bit+ 
                              +shunt-adc-12bit+ 
                              +mode-continuous+)))
    ;; Write configuration to config register
    (%write-register +config-register+ config-value)
    
    ;; Calculate and write calibration value
    (let ((calibration-value (%calculate-calibration)))
      (%write-register +calibration-register+ calibration-value))))

(defun %calculate-calibration ()
  "Calculate calibration value for current measurement"
  (let* ((current-lsb (/ *max-currency* 32767))
         (calibration (truncate (/ 0.04096 (* current-lsb *shunt-ohm*)))))
    calibration))

(defun %write-register (register value)
  "Write a 16-bit value to an INA219 register using word write"
  (cl-pigpio:i2c-write-word-to-register *i2c-handle* register value :swap-bytes t))

(defun %read-register (register)
  "Read a 16-bit value from an INA219 register using word read"
  (cl-pigpio:i2c-read-word-from-register *i2c-handle* register :swap-bytes t))

(defun %read-current-raw ()
  "Read raw current value from INA219"
  (let ((raw-value (%read-register +current-register+)))
    ;; Convert from two's complement if negative
    (if (> raw-value 32767)
        (- raw-value 65536)
        raw-value)))

(defun %raw-to-current (raw-value)
  "Convert raw current reading to actual current in Amperes"
  (let ((current-lsb (/ *max-currency* 32767)))
    (* raw-value current-lsb)))

(defun %next-currency-value (times)
  "Take multiple current readings and return the average"
  (let ((total 0))
    (dotimes (i times)
      (let ((raw-current (%read-current-raw)))
        (incf total (%raw-to-current raw-current)))
      ;; Small delay between readings
      (sleep 0.05))
    (/ total times)))

(defun read-currency ()
  "Read current value from INA219, returning average of 10 readings"
  (handler-case
      (let ((current (%next-currency-value 10)))
        (values :ok (* current 1000)))
    (error (e)
      (values :error (format nil "Failed to read current: ~A" e)))))

(defun cleanup ()
  "Clean up I2C connection"
  (when *i2c-handle*
    (cl-pigpio:i2c-close *i2c-handle*)
    (setf *i2c-handle* nil)))
