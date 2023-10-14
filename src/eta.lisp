(defpackage :cl-eta.eta
  (:use :cl)
  (:nicknames :eta)
  (:export #:eta-init
           #:eta-stop
           #:*eta-serial-proxy-impl*
           #:*eta-serial-device*))

(in-package :cl-eta.eta)

;; ---------------------
;; eta-serial
;; ---------------------

(defvar *eta-serial-device* "/dev/ttyUSB0")
(defvar *eta-serial-port* nil)
(defvar *eta-serial-proxy-impl* nil)
(defvar +eta-new-empty-data+ #())

(defun %process-complete-pkg (pkg-data)
  "Transmits monitor items to openhab.
Returns monitor items, car item name, cdr item value."
  (multiple-value-bind (pkg-type items)
      (eta-pkg:extract-pkg pkg-data)
    (case pkg-type
      (:fail (progn
               (log:warn "Failed package extraction: ~a" pkg-data)
               nil))
      (:eta-monitor (progn
                      (log:info "Monitor data: ~a" pkg-data)
                      (dolist (item items)
                        (log:info "Posting item: ~a, value: ~a" (car item) (cdr item)))
                        ;(openhab:do-post (car item) (cdr item)))
                      items))
      (otherwise (progn
                   (log:info "Unknown extract pkg result!")
                   nil)))))

(defun eta-destroy ()
  (setf *eta-serial-proxy-impl* nil))

(defun eta-init ()
  (setf *eta-serial-port*
        (eta-ser-if:open-serial *eta-serial-proxy-impl* *eta-serial-device*)))

(defun eta-close ()
  (eta-ser-if:close-serial *eta-serial-proxy-impl* *eta-serial-port*))

(defun eta-write (data)
  (eta-ser-if:write-serial *eta-serial-proxy-impl* *eta-serial-port* data))

(defvar *eta-serial-data* nil)
(defun eta-read (actor)
  (let ((result
          (eta-ser-if:read-serial *eta-serial-proxy-impl* *eta-serial-port*)))
    (log:debug "eta read result: ~a" result)
    (let* ((serial-data *eta-serial-data*)
           (new-serial-data
             (handler-case
                 (multiple-value-bind (complete data)
                     (eta-pkg:collect-data serial-data read-data)
                   (if complete
                       (let ((mon-items (%process-complete-pkg data)))
                         +eta-new-empty-data+)
                       data))
               (error (c)
                 (progn
                   (log:warn "Error collecting data: ~a" c)
                   state)))))
      (setf *eta-serial-data* new-serial-data))))
