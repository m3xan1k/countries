#lang racket

(require
 rackunit
 "../lib/countries.rkt")

(define-test-suite countries-test
  (test-case "lookup exists"
             (let ((result (lookup-country "france")))
               (check-not-false result)
               (check-equal? (country-data-code result) "FR")
               (check-equal? (country-data-currency result) "EUR")
               (check-equal? (country-data-domain result) ".fr")))

  (test-case "lookup not exists"
             (check-false (lookup-country "atlantis")))

  (test-case "lookup string normalized"
    (check-not-false (lookup-country " uNiTed sTates  "))))

(module+ test
  (require rackunit/text-ui)
  (run-tests countries-test))

