;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_187) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 187.

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

; ListOfGamePlayers is one of:
; - '()
; - (cons (make-gp String Number) ListOfGamePlayers)

; ListOfGamePlayers -> ListOfGamePlayers
; sorts list of game players by their score in descending order

(check-expect (sort-gp> '()) '())
(check-expect
 (sort-gp> (list (make-gp "simon" 10) (make-gp "alex" 20)))
 (list (make-gp "alex" 20) (make-gp "simon" 10)))

(define (sort-gp> logp)
  (cond
    [(empty? logp) '()]
    [(cons? logp)
     (insert-gp (first logp) (sort-gp> (rest logp)))]))

; GamePlayer ListOfGamePlayers -> ListOfGamePlayers
; inserts the gameplayer into (sort-gp> listofgameplayers)

(check-expect
 (insert-gp (make-gp "simon" 10) '())
 (list (make-gp "simon" 10)))

(check-expect
 (insert-gp
  (make-gp "simon" 10) (list (make-gp "sara" 20) (make-gp "alex" 4)))
 (list (make-gp "sara" 20) (make-gp "simon" 10) (make-gp "alex" 4)))

(define (insert-gp p logp)
  (cond
    [(empty? logp) (cons p '())]
    [(cons? logp)
     (if (gp> p (first logp)) (cons p logp)
         (cons (first logp) (insert-gp p (rest logp))))]))

; GamePlayer GamePlayer -> Boolean
; determines if one gameplayer's score is higher than the other's

(check-error (gp> (make-gp "sara" 20) 10))
(check-expect (gp> (make-gp "sara" 20) (make-gp "sam" 10)) #true)
(check-expect (gp> (make-gp "sara" 10) (make-gp "sam" 20)) #false)

(define (gp> p1 p2)
  (cond
    [(and (gp? p1) (gp? p2))
     (> (gp-score p1) (gp-score p2))]
    [else
     (error "p1 and p2 must be (make-player String Number)")]))
     
