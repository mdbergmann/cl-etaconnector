(defpackage :cl-eta.eta
  (:use :cl :gs-user :eta-ser-if :eta-col)
  (:nicknames :eta)
  (:export #:init-serial
           #:start-record
           #:ensure-initialized
           #:ensure-shutdown
           #:*serial-proxy-impl*
           #:*eta-collector-impl*
           #:*openhab-impl*))

(in-package :cl-eta.eta)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy-impl* nil)
(defvar *eta-collector-impl* nil)

(defun ensure-initialized ()
  (unless *eta-collector-impl*
    (setf *eta-collector-impl* :prod))
  (unless *serial-proxy-impl*
    (setf *serial-proxy-impl* :prod))
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA-serial-actor"
                                      :state (new-start-pkg)
                                      :receive (lambda (self msg state)
                                                 (%serial-actor-receive self msg state)))))
  (values *serial-actor* *actor-system*))

(defun ensure-shutdown ()
  (when *actor-system*
    (ac:shutdown *actor-system* :wait t))
  (setf *actor-system* nil)
  (setf *serial-actor* nil)
  (setf *serial-proxy-impl* nil)
  (setf *eta-collector-impl* nil))

(defun init-serial (device)
  (multiple-value-bind (actor)
      (ensure-initialized)
    (setf *serial-device* device)
    (let ((ask-result (act:ask-s actor '(:init . nil))))
      (cond
        ((listp ask-result)
         (case (car ask-result)
           (:handler-error (values :fail (format nil "~a" (cdr ask-result))))
           (otherwise (values :ok))))
        (t (values :ok))))))

;; ---------------------
;; package functions
;; ---------------------

(defun start-record ()
  "Triggers the recording of data.
Once this command is sent, the ETA will start to send monitor data packages.
So we gotta trigger a read here as well."
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor '(:write . "Foo"))
    (act:tell actor '(:read . nil)))
  :ok)

;; ---------------------
;; actor receive
;; ---------------------

(defun new-start-pkg () #())

(defun handle-complete-pkg (pkg-data)
  (multiple-value-bind (pkg-type items)
      (eta-extract:extract-pkg pkg-data)
    (when (eq :monitor pkg-type)
      (dolist (item items)
        (openhab:do-post (car item) (cdr item))))))

(defun generate-new-state (old-state)
  (multiple-value-bind (complete data)
      (collect-data *eta-collector-impl*
                    old-state
                    (read-serial *serial-proxy-impl* *serial-port*))
    (if complete
        (progn 
          (handle-complete-pkg data)
          (new-start-pkg))
        data)))

(defun %serial-actor-receive (self msg state)
  (let ((resp
          (case (car msg)
            (:init
             (cons
              (setf *serial-port*
                    (open-serial *serial-proxy-impl* *serial-device*))
              state))
            (:write
             (cons (write-serial *serial-proxy-impl* *serial-port* (cdr msg)) state))
            (:read
             (let ((new-state (generate-new-state state)))
               (act:tell self '(:read . nil))
               (cons t new-state))))))
    resp))
