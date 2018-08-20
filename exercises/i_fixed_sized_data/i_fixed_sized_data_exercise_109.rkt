;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_109) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 109.

(require 2htdp/universe)

(define WIDTH 100)
(define HEIGHT 100)

; Command is a String and can be a combination of
; the following 1Strings
; - "a", "b", "c", "d"

; ExpectsToSee is one of:
; – AA
; – BB
; – DD 
; – ER 

(define AA "s")
(define AA-RENDER (empty-scene WIDTH HEIGHT))

(define BB "x")
(define BB-RENDER
  (rectangle
   (image-width AA-RENDER)
   (image-height AA-RENDER) "solid" "yellow"))

(define DD "f")
(define DD-RENDER
  (rectangle
   (image-width AA-RENDER)
   (image-height AA-RENDER) "solid" "green"))

(define ER "e")
(define ER-RENDER
  (rectangle
   (image-width AA-RENDER)
   (image-height AA-RENDER) "solid" "red"))

(define-struct executor [keys state])
; Parser is a structure:
;  (make-parser Command ExpectsToSee)
; interpretation:
;  (make-parser "abc" BB) is a parser with key

(define INIT
  (make-executor "" AA))

(define (main e)
  (big-bang e
    [to-draw render]
    [on-key change-state]))

; ExpectsToSee -> Image
; renders the command response; 
;; tests
(check-expect
 (render (make-executor "" AA)) AA-RENDER)

(check-expect
 (render (make-executor "a" BB)) BB-RENDER)

(check-expect
 (render (make-executor "ay" ER)) ER-RENDER)

(check-expect
 (render (make-executor "abcd" DD)) DD-RENDER)

(define (render e)
  (cond
    [(equal? (executor-state e) AA) AA-RENDER]
    [(equal? (executor-state e) BB) BB-RENDER]
    [(equal? (executor-state e) DD) DD-RENDER]
    [(equal? (executor-state e) ER) ER-RENDER]))

; ExpectsToSee KeyEvent -> ExpectsToSee
; changes the expect to see e based on the key
; event ke.
;; tests
(check-expect
 (change-state (make-executor "" AA) "a")
 (make-executor "a" BB))

(check-expect
 (change-state (make-executor "" AA) "y")
 (make-executor "" ER))

(check-expect
 (change-state (make-executor "a" BB) "b")
 (make-executor "ab" BB))

(check-expect
 (change-state (make-executor "abcbc" BB) "b")
 (make-executor "abcbcb" BB))

(check-expect
 (change-state (make-executor "abcbc" BB) "y")
 (make-executor "abcbc" ER))

(check-expect
 (change-state (make-executor "abcbc" ER) "b")
 (make-executor "abcbc" ER))

(check-expect
 (change-state (make-executor "" ER) "b")
 (make-executor "" ER))

(check-expect
 (change-state (make-executor "" ER) "a")
 (make-executor "" ER))

(check-expect
 (change-state (make-executor "abcbc" ER) "u")
 (make-executor "abcbc" ER))

(check-expect
 (change-state (make-executor "abcbc" ER) "d")
 (make-executor "abcbc" ER))

(define (change-state e ke)
  (cond
    [(equal? (executor-state e) AA)
     (cond
       [(string=? ke "a") (make-executor ke BB)]
       [else (make-executor "" ER)])]
    [(equal? (executor-state e) BB)
    (cond
       [(or (string=? ke "b") (string=? ke "c"))
        (make-executor
         (string-append (executor-keys e) ke) BB)]
       [(string=? ke "d")
        (make-executor
         (string-append (executor-keys e) ke) DD)]
       [else
        (make-executor (executor-keys e) ER)])]
     [(or (equal? (executor-state e) DD)
          (equal? (executor-state e) ER)) e]))