(defpackage :cl-eta.package-test
  (:use :cl :fiveam :cl-eta.package)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.package-test)

(def-suite package-tests
  :description "Tests for eta package handling"
  :in cl-eta.tests:test-suite)

(in-suite package-tests)

(test collect-data--empty-empty
  (is (equalp '(nil #()) (multiple-value-list (collect-data #() #())))))

(test collect-data--half-package
  (is (equalp '(nil #(123 1 2)) (multiple-value-list (collect-data #(123 1) #(2))))))

(test collect-data--full-package
  (is (equalp '(t #(123 1 2 125)) (multiple-value-list (collect-data #(123 1) #(2 125))))))

(test collect-data--double-full-package
  (is (equalp '(t #(123 1 2 125)) (multiple-value-list (collect-data #() #(123 1 2 125 123 1 2 125))))))

(test collect-data--full-package-in-middle
  (is (equalp '(t #(123 1 2 125)) (multiple-value-list (collect-data #(1 4 7 8) #(123 1 2 125 10 23))))))

(test collect-data--full-package-multiple-starts
  (is (equalp '(t #(123 1 2 125)) (multiple-value-list (collect-data #() #(123 2 3 123 1 2 125))))))

(test extract-pkg--fail-empty
  (is (equalp '(:fail "Undersized package!") (multiple-value-list (extract-pkg #())))))

(test extract-pkg--monitor--one-item
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" 54.5)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  0 ; node id
                  0 167 ; monitor id two byte
                  2 33 ; value two byte (545) / divisor 10
                  125))))))

(test extract-pkg--monitor--one-item--negative-value
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" -2.6)
                                       (cons "EtaBoiler" -2.1)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  0 ; node id
                  0 167 ; monitor id two byte
                  255 229 ; negative value two byte (-2.6) / divisor 10
                  0
                  0 21
                  255 234 ; negative (-2.1)
                  125))))))

#|
#(123 77 68 65 203 24 0 19 5 115 24 0 20 2 226 24 0 21 0 0 24 0
22 255 229 24 0 23 1 77 24 0 53 81 31 24 0 77 2 211 24 0 78 1
36 24 0 107 55 45 24 0 112 2 84 24 0 167 0 0 32 0 75 0 191 32
0 145 1 122 125)
#(123 77 68 65 32 24 0 19 5 120 24 0 20 2 232 24 0 21 0 0 24 0
22 255 231 24 0 23 1 119 24 0 53 81 31 24 0 77 2 220 24 0 78
1 38 24 0 107 55 45 24 0 112 2 89 24 0 167 0 0 32 0 75 0 188
32 0 145 1 139 125)
|#

(test extract-pkg--monitor--more-items
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" 54.5)
                                       (cons "EtaBoiler" 12.4)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  10 ; payload len
                  3 ; checksum of payload (ignored)
                  0 ; node id
                  0 167 ; monitor id two byte
                  2 33 ; value two byte (545) / divisor 10
                  0
                  0 21
                  0 124
                  125))))))

(test extract-pkg--monitor--wrong-payload-size
  (is (equalp (list :fail "Wrong payload size!")
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  2 33 ; value two byte (545) / divisor 10
                  125))))))

(test extract-pkg--production
  (is (equalp (list :FAIL "Wrong payload size!")
              (multiple-value-list
               (extract-pkg
                #(123 77 68 65 14 24 0 19 1 153 24 0 20 1 157 24 0
                  21 0 0 24 0 22 0 78 24 0 23 1 69 24 0 53 85 85
                  24 0 77 1 199 24 0 78 1 18 24 0 107 57 52 24 0
                  112 1 76 24 0 167 0 0 32 0 75 1 185 32 0 145 1
                  105 125 123 77 68 65 18 24 0 19 1 153 24 0 20 1
                  157 24 0 21 0 0 24 0 22 0 79 24 0 23 1 73 24 0
                  53 85 85 24 0 77 1 198 24 0 78 1 18 24 0 107 57
                  52 24 0 112 1 76 24 0 167 0 0 32 0 75 1 185 32 0
                  145 1 105 125))))))

(test extract-pkg--production2
  (is (equalp (list :FAIL "Wrong payload size!")
              (multiple-value-list
               (extract-pkg
                #(123 77 68 65 15 24 0 19 5 139 24 0 20 2 224 24
                  57 111 24 0 112 2 89 24 0 167 0 0 32 0 75 0 175
                  32 0 145 1 70 125))))))


;; todo: other package types
