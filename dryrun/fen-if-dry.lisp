(defpackage :cl-eta.fen-if
  (:use :cl)
  (:nicknames :fen-if)
  (:export #:read-item))

(in-package :cl-eta.fen-if)

(defparameter *fen-url* "http://192.168.50.128:8084/rest/channel/~a")

(defun read-item (rest-path)
  (values :ok 123))
