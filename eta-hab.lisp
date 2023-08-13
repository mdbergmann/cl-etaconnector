(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-eta))

(defpackage :eta-hab
  (:use :cl :hab))
(in-package :eta-hab)

(defconfig)
