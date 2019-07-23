;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_262) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 262.

; Number -> [List-of [List-of Number]]
; for the given number n produces diagonal squares
; of 0s and 1s

(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identityM n)
  (local (
          (define (expand m)
            (if (< m n)
                (cons (create-list 0 m) (expand (add1 m)))
                '()))
          (define (create-list u w)
            (cond
              [(= u n) '()]
              [else
               (cons (if (= u w) 1 0) (create-list (add1 u) w))])))
    (expand 0)))