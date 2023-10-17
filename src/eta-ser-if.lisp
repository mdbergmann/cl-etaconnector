(defpackage :cl-eta.eta-ser-if
  (:use :cl)
  (:nicknames :eta-ser-if)
  (:export #:open-serial
           #:close-serial
           #:write-serial
           #:read-serial))

(in-package :cl-eta.eta-ser-if)


(defun open-serial (device)
  (cserial-port:open-serial device
                            :baud-rate 19200
                            :data-bits 8
                            :stop-bits 1
                            :parity :none))

(defun close-serial (port)
  (cserial-port:close-serial port))

(defun write-serial (port data)
  (cserial-port:write-serial-byte-vector data port))

(defun read-serial (port &optional (timeout 2000))
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (handler-case
        (let ((nread (cserial-port:read-serial-byte-vector buf port :timeout-ms timeout)))
          (case nread
            ('() #())
            (otherwise (subseq buf 0 nread))))
      (error ()
        ;; timeout?
        #()))))
