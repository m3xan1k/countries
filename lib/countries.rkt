#lang racket

(provide
 country-data
 country-data-code
 country-data-currency
 country-data-domain
 lookup-country)

;; A structure representing country information.
;; - code: The two-letter country code.
;; - currency: The three-letter currency code.
;; - domain: The top-level internet domain for the country.
(struct country-data (code currency domain) #:transparent)

;; Looks up country information by name.
;;
;; Arguments:
;;   name: A string representing the country name (case-insensitive).
;;
;; Returns:
;;   A country-data struct if the country is found, #f otherwise.
;;
;; Example:
;;   (lookup-country "France") => #(struct:country-data "FR" "EUR" ".fr")
;;   (lookup-country "atlantis") => #f
(define (lookup-country name)
  (hash-ref country-db (string-titlecase (string-trim name)) #f))

;; Database of country information.
(define country-db
  (make-hash
   (list (cons "United States" (country-data "US" "USD" ".us"))
         (cons "United Kingdom" (country-data "GB" "GBP" ".uk"))
         (cons "France" (country-data "FR" "EUR" ".fr"))
         (cons "Germany" (country-data "DE" "EUR" ".de"))
         (cons "Japan" (country-data "JP" "JPY" ".jp")))))


