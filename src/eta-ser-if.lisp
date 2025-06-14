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

(defun read-serial (port &optional (timeout nil))
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (handler-case
        (let ((byte (cserial-port:read-serial-byte
                     port
                     :timeout-ms
                     (or timeout
                         cserial-port::*default-timeout-ms*))))
          (case byte
            (nil #())
            (otherwise (vector byte))))
        ;; (let ((nread (cserial-port:read-serial-byte-vector
		;;       buf port
		;;       :timeout-ms
		;;       (or timeout
		;; 	  cserial-port::*default-timeout-ms*))))
        ;;   (case nread
        ;;     (nil #())
        ;;     (otherwise (subseq buf 0 nread))))
      )))
