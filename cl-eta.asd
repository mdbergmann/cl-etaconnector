(defsystem "cl-eta"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("cl-hab"
               "cserial-port"
               "drakma"
               "bit-smasher"
               "py4cl"
               )
  :components ((:module "src"
                :serial t
                :components
                ((:file "openhab")
                 #-:darwin (:file "ina219-if")
                 #+:darwin (:file "ina219-if-dummy")
                 (:file "eta-ser-if")
                 (:file "solar-if")
                 (:file "eta-pkg")
                 (:file "eta")
                 (:file "eta-helper")
                 )))
  :in-order-to ((test-op (test-op "cl-eta/tests"))))

(defsystem "cl-eta/tests"
  :author "Manfred Bergmann"
  :depends-on ("cl-eta"
               "fiveam"
               "cl-mock"
               "hunchentoot"
               "easy-routes"
               )
  :components ((:module "test"
                :components
                ((:file "all-tests")
                 (:file "eta-test")
                 (:file "eta-helper-test")
                 (:file "eta-pkg-test")
                 )))
  :description "Test system for cl-eta"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-eta.tests))))
;; load system
;; (asdf:load-system "cl-eta")
;;

#|
TODO:
OK - test for read continously
OK - test for call to read handler when data arrived
OK - test for incomplete package handling
OK - test for complete package handling
OK - complete package handling should call eta pkg extractor
OK - result of pkg extractor should extract eta package
OK - extracted package should send openhab post requests for each extract
OK - verify proper eta-packages are used (i.e. for start-record)
OK - log extracted package
OK - implement full start-record package
OK - update atest with receive monitor package
OK - 'stop-record'
OK - 'shutdown-serial
OK - implement real http server for more integration testing for http post call
OK - calculate op hours and ignition avgs daily and weekly
OK - report avgs
OK - calculate avgs
OK - reset avg after report
OK - error handling for drakma request
OK - add stop/shutdown to eta, ina and solar
OK - allow actors to register cron jobs via post-start event via event-stream
OK - store and load state of eta actor

- filter temp values for spikes
- implement more receive package types (error, etc)
|#
