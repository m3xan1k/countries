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
         (cons "Japan" (country-data "JP" "JPY" ".jp"))
         (cons "Canada" (country-data "CA" "CAD" ".ca"))
         (cons "Australia" (country-data "AU" "AUD" ".au"))
         (cons "Italy" (country-data "IT" "EUR" ".it"))
         (cons "Spain" (country-data "ES" "EUR" ".es"))
         (cons "Netherlands" (country-data "NL" "EUR" ".nl"))
         (cons "Sweden" (country-data "SE" "SEK" ".se"))
         (cons "Switzerland" (country-data "CH" "CHF" ".ch"))
         (cons "Norway" (country-data "NO" "NOK" ".no"))
         (cons "Denmark" (country-data "DK" "DKK" ".dk"))
         (cons "Finland" (country-data "FI" "EUR" ".fi"))
         (cons "Belgium" (country-data "BE" "EUR" ".be"))
         (cons "Austria" (country-data "AT" "EUR" ".at"))
         (cons "Portugal" (country-data "PT" "EUR" ".pt"))
         (cons "Greece" (country-data "GR" "EUR" ".gr"))
         (cons "Ireland" (country-data "IE" "EUR" ".ie"))
         (cons "New Zealand" (country-data "NZ" "NZD" ".nz"))
         (cons "South Korea" (country-data "KR" "KRW" ".kr"))
         (cons "China" (country-data "CN" "CNY" ".cn"))
         (cons "India" (country-data "IN" "INR" ".in"))
         (cons "Brazil" (country-data "BR" "BRL" ".br"))
         (cons "Mexico" (country-data "MX" "MXN" ".mx"))
         (cons "Argentina" (country-data "AR" "ARS" ".ar"))
         (cons "South Africa" (country-data "ZA" "ZAR" ".za"))
         (cons "Russia" (country-data "RU" "RUB" ".ru"))
         (cons "Turkey" (country-data "TR" "TRY" ".tr"))
         (cons "Indonesia" (country-data "ID" "IDR" ".id"))
         (cons "Thailand" (country-data "TH" "THB" ".th"))
         (cons "Malaysia" (country-data "MY" "MYR" ".my"))
         (cons "Singapore" (country-data "SG" "SGD" ".sg"))
         (cons "Philippines" (country-data "PH" "PHP" ".ph"))
         (cons "Vietnam" (country-data "VN" "VND" ".vn"))
         (cons "Israel" (country-data "IL" "ILS" ".il"))
         (cons "United Arab Emirates" (country-data "AE" "AED" ".ae"))
         (cons "Saudi Arabia" (country-data "SA" "SAR" ".sa"))
         (cons "Egypt" (country-data "EG" "EGP" ".eg")))))

