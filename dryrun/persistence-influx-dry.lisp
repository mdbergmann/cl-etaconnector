(defpackage :cl-hab.influx-persistence
  (:use :cl :cl-hab.persistence)
  (:nicknames :influx-persistence)
  (:import-from #:persp
                #:persistence)
  (:export #:influx-persistence
           #:make-influx-persistence)
  )

(in-package :cl-hab.influx-persistence)

;; ---------------------------------------
;; Writing
;; ---------------------------------------

(defmethod persist ((persistence influx-persistence) item)
  (log:debug "dry: Persisting item: ~a" item)
  (log:info "dry: Persisted item OK: ~a" item))

(defmethod retrieve ((persistence influx-persistence) item)
  (log:debug "dry: Reading item: ~a" item)
  (make-persisted-item :value (random 100) :timestamp (get-universal-time)))

(defmethod retrieve-range ((persistence influx-persistence) item range)
  (log:debug "dry: Reading item with range: ~a" item)
  (list
   (make-persisted-item :value (random 100) :timestamp (get-universal-time))
   (make-persisted-item :value (random 100) :timestamp (get-universal-time))))
