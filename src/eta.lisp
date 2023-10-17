(defpackage :cl-eta.eta
  (:use :cl)
  (:nicknames :eta)
  (:export #:eta-init
           #:eta-close
           #:eta-write
           #:eta-read-monitors
           #:*eta-serial-device*))

(in-package :cl-eta.eta)

(defvar *eta-serial-device* "/dev/ttyUSB0")
(defvar *eta-serial-port* nil)
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

(defun eta-init ()
  (setf *eta-serial-port*
        (eta-ser-if:open-serial *eta-serial-device*))
  :ok)

(defun eta-close ()
  (eta-ser-if:close-serial *eta-serial-port*)
  :ok)

(defun eta-write (data)
  (values :ok (eta-ser-if:write-serial *eta-serial-port* data)))

(defvar *eta-serial-data* nil)
(defun eta-read-monitors ()
  (let ((read-data
          (eta-ser-if:read-serial *eta-serial-port*)))
    (log:debug "eta read result: ~a" read-data)
    (let* ((serial-data *eta-serial-data*)
           (new-serial-data
             (multiple-value-bind (complete data)
                 (eta-pkg:collect-data serial-data read-data)
               (if complete
                   (progn
                     (%process-complete-pkg data)
                     +eta-new-empty-data+)
                   data))))
      (setf *eta-serial-data* new-serial-data))))
