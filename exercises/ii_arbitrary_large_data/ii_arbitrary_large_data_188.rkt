;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_188) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 188.

(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time 

; ListOfEmails is one of:
; - '()
; - (cons (make-email String Number String) ListOfEmails)

; ListOfEmails -> ListOfEmails
; sorts list of emails by date in descending order

(check-expect (sort-email> '()) '())
(check-expect
 (sort-email>
  (list (make-email "sara" 10 "simon") (make-email "alex" 20 "simon")))
 (list (make-email "alex" 20 "simon") (make-email "sara" 10 "simon")))

(define (sort-email> loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe)
     (insert-email (first loe) (sort-email> (rest loe)))]))

; Email ListOfEmails -> ListOfEmails
; inserts email e into (sort-email> listofemails)

(check-expect
 (insert-email (make-email "simon" 10 "sara") '())
 (list (make-email "simon" 10 "sara")))

(check-expect
 (insert-email
  (make-email "simon" 10 "sara")
  (list (make-email "sara" 20 "alex") (make-email "alex" 4 "simon")))
 (list
  (make-email "sara" 20 "alex")
  (make-email "simon" 10 "sara")
  (make-email "alex" 4 "simon")))

(define (insert-email e loe)
  (cond
    [(empty? loe) (cons e '())]
    [(cons? loe)
     (if (email> e (first loe)) (cons e loe)
         (cons (first loe) (insert-email e (rest loe))))]))

; Email Email -> Boolean
; determines if one emails's date is higher than the other's

(check-error (email> (make-email "sara" 20 "alex") 10))
(check-expect
 (email>
  (make-email "sara" 20 "alex") (make-email "sam" 10 "sara")) #true)

(check-expect
 (email>
  (make-email "sara" 10 "alex") (make-email "sam" 20 "sara")) #false)

(define (email> e1 e2)
  (cond
    [(and (email? e1) (email? e2))
     (> (email-date e1) (email-date e2))]
    [else
     (error "e1 and e2 must be (make-email String Number String)")]))

; ListOfEmails -> ListOfEmails
; sorts list of emails by name in ascending order

(check-expect (sort-email</name '()) '())
(check-expect
 (sort-email</name
  (list (make-email "sara" 10 "simon") (make-email "alex" 20 "simon")))
 (list (make-email "alex" 20 "simon") (make-email "sara" 10 "simon")))

(define (sort-email</name loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe)
     (insert-email/name (first loe) (sort-email</name (rest loe)))]))

; Email ListOfEmails -> ListOfEmails
; inserts email into (sort-email</name listofemails)

(check-expect
 (insert-email/name (make-email "simon" 10 "sara") '())
 (list (make-email "simon" 10 "sara")))

(check-expect
 (insert-email/name
  (make-email "peter" 10 "sara")
  (list (make-email "alex" 4 "simon") (make-email "sara" 20 "alex")))
 (list
  (make-email "alex" 4 "simon")
  (make-email "peter" 10 "sara")
   (make-email "sara" 20 "alex")))

(define (insert-email/name e loe)
  (cond
    [(empty? loe) (cons e '())]
    [(cons? loe)
     (if (email</name e (first loe)) (cons e loe)
         (cons (first loe) (insert-email/name e (rest loe))))]))

; Email Email -> Boolean
; determines if one emails's name is alphabetically lower order than
; the other's

(check-error (email</name (make-email "sara" 20 "alex") 10))
(check-expect
 (email</name
  (make-email "sara" 20 "alex") (make-email "alice" 10 "sara")) #false)

(check-expect
 (email</name
  (make-email "sara" 10 "alex") (make-email "zoe" 20 "sara")) #true)

(define (email</name e1 e2)
  (cond
    [(and (email? e1) (email? e2))
     (string<? (email-from e1) (email-from e2))]
    [else
     (error "e1 and e2 must be (make-email String Number String)")]))