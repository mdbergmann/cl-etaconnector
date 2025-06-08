(defpackage :cl-eta.shelly-pro-3em
  (:use :cl)
  (:nicknames :shelly-pro-3em)
  (:export #:read-power))

(in-package :cl-eta.shelly-pro-3em)

(defparameter *cellar-url* "http://192.168.50.199/rpc")

(defun read-power ()
  (log:info "dry: Reading cellar heizstab power...")
  (values :ok 12.3 12,4 12,5 37.2))
