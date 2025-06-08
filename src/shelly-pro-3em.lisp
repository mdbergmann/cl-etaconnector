(defpackage :cl-eta.shelly-pro-3em
  (:use :cl)
  (:nicknames :shelly-pro-3em)
  (:export #:read-power
           #:read-accumulated-total-power))

(in-package :cl-eta.shelly-pro-3em)

(defparameter *cellar-url* "http://192.168.50.199/rpc")

(defun read-power ()
  "Returns three values:
1. `:ok' on success (where also 2. is provided), 
   `:nok' on failure (additionally provided is the HTTP status code).
2. 'a_act_power, b_act_power, c_act_power, total_act_power' in W/h"
  (multiple-value-bind (body stat)
      (drakma:http-request
       (format nil "~a/EM.GetStatus?id=0" *cellar-url*)
       :method :get)
    (case stat
      (200 (let* ((json (yason:parse (babel:octets-to-string body)))
                  (a-act-power (gethash "a_act_power" json))
                  (b-act-power (gethash "b_act_power" json))
                  (c-act-power (gethash "c_act_power" json))
                  (total-act-power (gethash "total_act_power" json)))
             (values :ok
                     (abs a-act-power)
                     (abs b-act-power)
                     (abs c-act-power)
                     (abs total-act-power))))
      (otherwise (values :nok stat)))))

(defun read-accumulated-total-power ()
  "Returns three values:
1. `:ok' on success (where also 2. is provided), 
   `:nok' on failure (additionally provided is the HTTP status code).
2. 'total_act_ret' in W/h"
  (multiple-value-bind (body stat)
      (drakma:http-request
       (format nil "~a/Shelly.GetStatus" *cellar-url*)
       :method :get)
    (case stat
      (200 (let* ((json (yason:parse (babel:octets-to-string body)))
                  (emdata (gethash "emdata:0" json))
                  (total-accu-act-power (gethash "total_act_ret" emdata)))
             (values :ok
                     (abs total-accu-act-power))))
      (otherwise (values :nok stat)))))
