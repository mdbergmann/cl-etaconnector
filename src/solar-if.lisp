(defpackage :cl-eta.solar-if
  (:use :cl)
  (:nicknames :solar-if)
  (:export #:read-power))

(in-package :cl-eta.solar-if)

(defparameter *solar-url* "http://192.168.50.21/rpc")

(defun read-power ()
  "Returns three values:
1. `:ok' on success (where also 2. and 3. are provided), 
   `:nok' on failure (additionally provided is the HTTP status code).
2. 'apower' current power in Watts.
3. 'total' in W/h"
  (multiple-value-bind (body stat)
      (drakma:http-request
       *solar-url* 
       :method :post
       :content "{\"id\":1,\"method\":\"Switch.GetStatus\",\"params\":{\"id\":0}}")
    (case stat
      (200 (let* ((json (yason:parse (babel:octets-to-string body)))
                  (result (gethash "result" json))
                  (apower (gethash "apower" result))
                  (total (gethash "total" (gethash "aenergy" result))))
             (values :ok apower total)))
      (otherwise (values :nok stat)))))
