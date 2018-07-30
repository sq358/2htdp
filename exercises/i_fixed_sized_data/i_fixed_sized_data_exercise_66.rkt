; Exercise 66.

(define-struct movie [title producer year])
; make-movie
; movie-title
; movie-producer
; movie-year
; movie?

(make-movie "Fargo" "Ethan Coen" "1996")

(define-struct person [name hair eyes phone])
; make-person
; person-name
; person-hair
; person-eyes
; person-phone
; person?

(make-person
 "Governer" "Black" "Black" "+1334567890")

(define-struct pet [name number])
; make-pet
; pet-name
; pet-number
; pet?

(make-pet "Scotch" "122")

(define-struct CD [artist title price])
; make-CD
; CD-artist
; CD-title
; CD-price
; CD?

(make-CD
 "Roger Waters" "The final cut" "$35")

(define-struct sweater [material size producer])
; make-sweater
; sweater-material
; sweater-size
; sweater-producer

(make-sweater "Cotton" "M" "Mr. Taylor")
