(defpackage :cl-eta.eta-ser-if
  (:use :cl)
  (:nicknames :eta-ser-if)
  (:export #:open-serial
           #:close-serial
           #:write-serial
           #:read-serial))

(in-package :cl-eta.eta-ser-if)

(defun open-serial (device)
  (log:info "dry: Opening serial port ~a" device))

(defun close-serial (port)
  (log:info "dry: Closing serial port ~a" port))

(defun write-serial (port data)
  (log:info "dry: Writing ~a to serial port ~a" data port))

(defun read-serial (port &optional (timeout 2000))
  (log:info "dry: Reading from serial port ~a" port)
  #(123
    77 68                          ; service identifier
    10                             ; payload len
    3                              ; checksum of payload (ignored)
    0                              ; node id
    0 167                          ; monitor id two byte
    2 33                           ; value two byte (545) / divisor 10
    0
    0 21
    0 124
    125))
