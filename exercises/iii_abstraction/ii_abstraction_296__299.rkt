;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_296__299) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 296.

(require 2htdp/image)
(require 2htdp/universe)

; Use compass-and-pencil drawings to check the tests in section 17.5

; Exercise 297.

(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

; Number Number Posn -> Number
; computes the distance between the points x,y, and p

(define (distance-between x y p)
  (sqrt (+ (sqr (- (posn-x p) x)) (sqr (- (posn-y p) y)))))

; Exercise 298.

(define (create-UFO-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))
 
(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))
 
; [N -> Image] Number -> Number
; shows the stream images at a rate of 30 images per second up
; to n images total

(define (my-animate s n)
  (big-bang 0
    [to-draw s]
    [on-tick add1 (/ 1 30) n]))

; Exercise 299.

; A Set of numbers is a function:
;  [Number -> Boolean]
; interpretation if s is a set and n is a number (s n)
; produces #true if n is in s, #false otherwise

; Set Number -> Set
; adds an element ed to a set

(define (set1 n)
  (member? n '(1 2 3)))

(check-satisfied 3 (add-element set1 4))
(check-satisfied 4 (add-element set1 4))

(define (add-element s n)
  (lambda (x)
    (or (s x) (= x n))))

; Set Set -> Set
; combines the elements of two given sets

(define (set3 n)
  (member? n '(1 2 3)))
(define (set4 n)
  (member? n '(4 5 6)))

(check-satisfied 1 (union set3 set4))
(check-expect ((union set3 set4) 7) #false)

(define (union s1 s2)
  (lambda (x)
    (or (s1 x) (s2 x))))

; Set Set -> Set
; collects all elements common to two sets

(define (set5 n)
  (member? n '(1 2 3 4)))
(define (set6 n)
  (member? n '(3 4 5 6)))

(check-satisfied 3 (intersect set5 set6))
(check-satisfied 4 (intersect set5 set6))
(check-expect ((intersect set5 set6) 1) #false) 

(define (intersect s1 s2)
  (lambda (x)
    (and (s1 x) (s2 x))))