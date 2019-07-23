;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname quote_unquite_234) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 234.

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers 
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))
                  
; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l

(require 2htdp/web-io)

(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(define title "Song rankings")

; List-of-strings -> List-of-rankings
; produces list of rankings for the given list of strings
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> List-of-rankings
; produces 
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-rankings -> ... nested list ...
; creates an html table for the list of rankings

(define (make-ranking lor)
  `(html
    (head
     (title ,title))
    (body
     (table ((border "1"))
            ,@(render-ranking (ranking lor))))))

; List-of-rankings -> ... nested list ...
; renders the list of rankings

(define (render-ranking lor)
  (cond
    [(empty? lor) '()]
    [else
     (cons
      `(tr (td ,(get-ranking (first lor)))
           (td ,(get-song-name (first lor))))
      (render-ranking (rest lor)))]))

(define (get-ranking r)
  (number->string (first r)))

(define (get-song-name r)
  (second r))