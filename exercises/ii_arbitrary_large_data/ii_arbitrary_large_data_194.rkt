;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_194) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 194.

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
  (render-poly.v4 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v4 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(define (render-poly.v4 img p)
  (connect-dots img (first p) p))

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

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; Image NELoP -> Image
; connects the dots in p by rendering lines in img

(check-expect (connect-dots MT (make-posn 30 20) (list (make-posn 10 10)))
              (scene+line MT 30 20 10 10 "red"))

(check-expect (connect-dots MT (make-posn 20 10) triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))

(check-expect (connect-dots MT (make-posn 10 10) square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))

(define (connect-dots img pn p)
  (cond
    [(empty? (rest p))
     (render-line MT (first p) pn)]
    [else
     (render-line
       (connect-dots img pn (rest p))
       (first p)
       (second p))]))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im

(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; Polygon -> Posn
; extracts the last item from p

(check-expect
 (last (list (make-posn 3 4) (make-posn 4 5) (make-posn 9 5)))
 (make-posn 9 5))

(check-expect
 (last
  (list (make-posn 3 4) (make-posn 4 5) (make-posn 9 5) (make-posn 3 4)))
 (make-posn 3 4))

(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Posn Polygons -> Polygons
; adds posn to the end of polygons

(check-expect
 (add-to-end (make-posn 5 6) triangle-p)
 (list
  (make-posn 20 10)
  (make-posn 20 20)
  (make-posn 30 20)
  (make-posn 5 6)))

(check-expect
 (add-to-end (make-posn 5 6) square-p)
 (list
  (make-posn 10 10)
  (make-posn 20 10)
  (make-posn 20 20)
  (make-posn 10 20)
  (make-posn 5 6)))

(define (add-to-end pn p)
  (cond
    [(empty? (rest (rest (rest p))))
     (list
      (first p) (second p) (third p) pn)]
    [else
     (cons (first p) (add-to-end pn (rest p)))]))