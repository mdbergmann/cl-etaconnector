(defpackage :cl-eta.solar-if
  (:use :cl)
  (:nicknames :solar-if)
  (:export #:read-power))

(in-package :cl-eta.solar-if)

(defparameter *solar-url* "http://192.168.50.190/rpc")

(defun read-power ()
  (log:info "dry: Reading solar power...")
  (values :ok 123 456))
