;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_191) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 191.

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; a plain background image
(define MT (empty-scene 50 50))

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
	
(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; Image Polygon -> Image
; renders the given polygon p into img

(check-expect
  (render-poly MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
       (render-line
         (render-line MT (first p) (second p))
         (second p) (third p))
       (third p) (first p))]
    [else
     (render-line (render-poly img (rest p))
                  (first p)
                  (second p))]))

; Image Posn Posn -> Image 
; renders a line from p to q into img

(check-expect
 (render-line MT (make-posn 2 4) (make-posn 5 6))
 (scene+line
  MT
  (posn-x (make-posn 2 4))
  (posn-y (make-posn 2 4))
  (posn-x (make-posn 5 6))
  (posn-y (make-posn 5 6))
  "red"))

(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; Image NELoP -> Image
; connects the dots in p by rendering lines in img

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 10 20 20 "red")
               20 20 30 20 "red"))

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red"))