#lang racket

(require "lib/countries.rkt")

(provide main)

(define (main)
  (printf "Country name: ")
  (let* ((input (read-line))
         (result (lookup-country input)))
    (if result
        (begin
          (printf "Code: ~a\n" (country-data-code result))
          (printf "Currency: ~a\n" (country-data-currency result))
          (printf "Domain: ~a\n" (country-data-domain result)))
        (printf "Country \"~a\" not found.\n" input))))

(module+ main (main))
