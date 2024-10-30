(defpackage :cl-eta.fen-if
  (:use :cl)
  (:nicknames :fen-if)
  (:export #:read-item))

(in-package :cl-eta.fen-if)

(defparameter *fen-url* "http://192.168.50.128:8084/rest/channel/~a")

(defun read-item (rest-path)
  "Returns the requested value as `values' like:

- `(values :ok <the-value>)' in case of success, or
- `(values :nok <http-status>)' in case of error."
  (multiple-value-bind (body stat)
      (drakma:http-request
       (format nil *fen-url* rest-path)
       :basic-authorization '("x" "owner"))
    (case stat
      (200 (let* ((json (yason:parse (babel:octets-to-string body)))
                  (value (gethash "value" json)))
             (values :ok value)))
      (otherwise (values :nok stat)))))

#|
CL-USER> (progn
(drakma:http-request "http://192.168.50.128:8084/rest/channel/ess0/Soc" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/ess0/ActivePower" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/charger10/ActualPower" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/charger10/ActualEnergy" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/charger11/ActualPower" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/charger11/ActualEnergy" :basic-authorization `("x" "owner"))
(drakma:http-request "http://192.168.50.128:8084/rest/channel/meter0/ActivePower" :basic-authorization `("x" "owner")))

"{\"address\":\"ess0/Soc\",\"type\":\"INTEGER\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"%\",\"value\":6}"
"{\"address\":\"ess0/ActivePower\",\"type\":\"INTEGER\",\"accessMode\":\"RO\",\"text\":\"Negative values for Charge; positive for Discharge\",\"unit\":\"W\",\"value\":1087}"
"{\"address\":\"charger10/ActualPower\",\"type\":\"INTEGER\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"W\",\"value\":0}"
"{\"address\":\"charger10/ActualEnergy\",\"type\":\"LONG\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"Wh_Σ\",\"value\":24096}"
"{\"address\":\"charger11/ActualPower\",\"type\":\"INTEGER\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"W\",\"value\":0}"
"{\"address\":\"charger11/ActualEnergy\",\"type\":\"LONG\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"Wh_Σ\",\"value\":0}"
"{\"address\":\"meter0/ActivePower\",\"type\":\"INTEGER\",\"accessMode\":\"RO\",\"text\":\"\",\"unit\":\"W\",\"value\":-14}"
|#
