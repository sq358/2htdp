;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_103) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exerc(ise 103.

; A SpaceNeed is Number.
; interpretation space need in cubic meters


(define-struct spider [legs space])
; A Spider is a structure:
;  (make-spider Number Space)
; interpretation (make-spider n s) is a spider with n number
; of legs that needs s cubic meters of space for transport

(define-struct elephant [space])
; An Elephant is a structure:
;  (make-elephant Space)
; interpretation (make-elephant n) is an elephant that
; needs n cubic meters of space for transport

(define-struct boa [length girth])
; A Boa constrictor is a structure:
;  (make-boa-constrictor Number Number Space)
; interpretation (make-boa l g) is a
; boa constrictor with length l meters and girth
; g centimeters

(define-struct armadillo [weight space])
; An Armadillo is a structure:
;  (make-armadillo Number Space)
; interpretation (make-armadillo w s) is an armadillo with
; weigth w kg that needs s cubic meters of space for tranport

; A ZooAnimal can be one of the following:
; - Spider
; - Elephant
; - Boa Constrictor
; - Armadillo

; (define (zoo-function-template a)
;  (cond
;    [(spider? a) ... (spider-legs a) ... (spider-space a)]
;    [(elephant? a) ... (elephant-space a)]
;    [(boa? a) ... (boa-length a) ... (boa-girth a)]
;    [(armadillo a) ... (armadillo-weigth a ) ... (armadillo-space a)]
;    ))

; ZooAnimal Number -> Boolean
; determines if the zoo animal a fits in cage with volume v
(check-expect
 (fits? (make-spider 4 .02) 1) #true)

(check-expect
 (fits? (make-elephant 12) 10) #false)

(check-expect
 (fits? (make-boa 1 20) 10) #true)

(check-expect
 (fits? (make-armadillo 10 5) 10) #true)

(check-expect
 (fits? (make-armadillo 10 10) 10) #true)

(define (fits? a s)
  (cond
    [(spider? a) (>= s (spider-space a))]
    [(elephant? a) (>= s (elephant-space a))]
    [(armadillo? a) (>= s (armadillo-space a))]
    [(boa? a) #true]
    ))


