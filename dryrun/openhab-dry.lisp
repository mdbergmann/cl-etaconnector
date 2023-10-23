(defpackage :cl-eta.openhab
  (:use :cl)
  (:nicknames :openhab)
  (:export #:do-post
           #:*openhab-base-url*))

(in-package :cl-eta.openhab)

(defun do-post (resource data)
  (log:debug "dry: posting item: ~a, value: ~a" resource data)
  :ok)
