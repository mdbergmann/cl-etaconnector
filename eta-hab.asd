;; Top-level starting point
;; run fresh repl and (asdf:load-system :eta-hab)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-asd "/home/manfred/quicklisp/local-projects/chipi/bindings/knx/binding-knx.asd"
                 :name "binding-knx"))

(defsystem "eta-hab"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("cl-eta"
               "chipi-web"
               "binding-knx")
  :components ((:file "eta-hab")))


;; caveat: (eta-helper:ina-init)  must be called after startup manually again.
