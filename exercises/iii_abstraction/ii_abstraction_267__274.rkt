;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_267__274) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 267.

(define USD-EUR-RATE 1.06)

; [List-of USD] -> [List-of EUR]
; converts list of USDs to list of EURs

(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 2 3 4))
              (list 2.12 3.18 4.24))

(define (convert-euro u)
  (cond
    [(empty? u) '()]
    [else
     (local ((define (usd->euro usd)
               (* USD-EUR-RATE usd)))
       (map usd->euro u))]))

; [List-of Posn] -> [List-of [List-of PairOfNumbers]]
; for the given list of posns produces list of list of
; pair of numbers

(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 2 3) (make-posn 1 4)))
              (list (list 2 3) (list 1 4)))

(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else
     (local ((define (posn->list p)
               (list (posn-x p) (posn-y p))))
       (map posn->list lop))]))

; Exercise 268.

(define-struct inventory [name description acq-price sales-price])

(define TEST-LOI
  (list (make-inventory "inv1" "inv1" 200 210)
        (make-inventory "inv2" "inv2" 180 250)
        (make-inventory "inv3" "inv3" 20 60)))

; [List-of Inventory] -> [List-of Inventory]
; sorts the list of inventories by the difference
; between two prices in descending order

(check-expect (sort-inv '()) '())
(check-expect (sort-inv TEST-LOI)
              (list (make-inventory "inv2" "inv2" 180 250)
                    (make-inventory "inv3" "inv3" 20 60)
                    (make-inventory "inv1" "inv1" 200 210)))

(define (sort-inv loi)
  (cond
    [(empty? loi) '()]
    [else
     (local ((define (insert i l cmp)
               (cond
                 [(empty? l) (list i)]
                 [else
                  (if (cmp i (first l))
                      (cons i l)
                      (cons (first l) (insert i (rest l) cmp)))]))
             (define (compare-price a b)
               (>= (price-diff a) (price-diff b)))
             (define (price-diff i)
               (abs (- (inventory-acq-price i) (inventory-sales-price i)))))
       (insert (first loi) (sort-inv (rest loi)) compare-price))]))

; Exercise 269.

; Number [List-of Inventory] -> [List-of Inventory]
; for the given number ua produces a list of items whose
; sales prices is below

(check-expect (eliminate-expensive 200 TEST-LOI)
              (list (make-inventory "inv3" "inv3" 20 60)))

(define (eliminate-expensive ua loi)
  (cond
    [(empty? loi) '()]
    [else
     (local ((define (expensive? i)
               (< (inventory-sales-price i) ua)))
       (filter expensive? loi))]))

; Exercise 270.

; N -> [List-of N]

(check-expect (ex.270-1 2) '(0 1))
(define (ex.270-1 n)
  (local ((define (fn a) a))
    (build-list n fn)))

; N -> [List-of N]

(check-expect (ex.270-2 2) '(1 2))
(define (ex.270-2 n)
  (build-list n add1))

; N -> [List-of N]
(check-expect (ex.270-3 3) '(1 1/2 1/3))
(define (ex.270-3 n)
  (local ((define (fn a)
            (/ 1 (add1 a))))
    (build-list n fn)))

; N -> [List-of N]
(check-expect (ex.270-4 3) '(0 2 4))
(define (ex.270-4 n)
  (local ((define (fn a)
           (* a 2)))
    (build-list n fn)))

; N -> [List-of [List-of N]]
(check-expect (ex.270-5 3)
              (list
               (list 1 0 0)
               (list 0 1 0)
               (list 0 0 1)))

(define (ex.270-5 n)
  (local ((define (fn b)
            (local ((define (generate-col a)
                      (if (= a b) 1 0)))
                    (build-list n generate-col))))
    (build-list n fn)))

; N -> [List-of N]

(check-within (tabulate 2 sqrt)
              (list (sqrt 0) (sqrt 1) (sqrt 2))
              .1)

(define (tabulate n R)
  (local ((define (fn a)
            (R a)))
  (build-list (add1 n) fn)))

; Exercise 271.

; String [List-of String] -> Boolean

(check-expect (find-name "bob" '("bob" "bobby" "blue"))
              #true)

(check-expect (find-name "bob" '("book" "pen"))
              #false)

(define (find-name n lon)
  (local ((define (starts-with? name)
            (and
             (<= (string-length n) (string-length name))
             (string=? n (substring name 0 (string-length name))))))
  (ormap starts-with? lon)))

;; Q. Should you use ormap or andmap to define a function that
;;  ensures that no name on some list exceeds a given width?
;; A. andmap should be use because the invarient should be true
;;  for all the list items

; Exercise 272.

; ListOfNumbers ListOfNumbers -> ListOfNumbers

(check-expect (append-from-fold '(1 2 3) '(4 5 6))
              '(1 2 3 4 5 6))

;; if foldl is used the output will be:
;;  (list 3 2 1 4 5 6)

(define (append-from-fold lon1 lon2)
  (foldr cons lon2 lon1))

; ListOfNumbers -> Number
; Computes the sum of numbers in the list

(check-expect (calculate-sum '(1 2 3)) 6)

(define (calculate-sum lon)
  (foldr + 0 lon))

; ListOfNumbers -> Number
; Computes the product of numbers in the list

(check-expect (calculate-product '(2 2 2)) 8)

(define (calculate-product lon)
  (foldr * 1 lon))

; ListOfImages -> Image
; Horizontally composes list of images

(define image1
  (circle 5 "solid" "red"))

(define image2
  (circle 5 "solid" "green"))

(check-expect (build-beside (list image1 image2))
              (beside image1 image2 empty-image))

(define (build-beside loi)
  (foldr beside empty-image loi))

;; Q. Can you use the other fold function?
;; A. Yes.


; ListOfImages -> Images
; stacks the list of images vertically

(check-expect (build-above (list image1 image2))
              (above image1 image2 empty-image))

(define (build-above loi)
  (foldr above empty-image loi))

; Exercise 273.

; [Any Any] [List-of Any] -> [List-of Any]
; produces a list for the given input list and function

(check-expect (mymap (lambda (x) (+ x 1)) '(1 2 3))
              '(2 3 4))

(define (mymap fn l)
  (foldr (lambda (x y) (cons (fn x) y)) '() l))

; Exercise 274.

; [List-of 1String] -> [List-of [List-of String]]
; produces the list of prefixes for the given list
; of 1strings

(check-expect (prefixes '()) '())

(check-expect
 (prefixes '("a")) '(("a")))

(check-expect
 (prefixes '("a" "b"))
 '(("a") ("a" "b")))

(check-expect (prefixes '("a" "b" "c" "d"))
              '(("a")
                ("a" "b")
                ("a" "b" "c")
                ("a" "b" "c" "d")))

(define (prefixes l)
  (build-list (length l)
              (lambda (x)
                (reverse
                 (foldl (lambda (y z) 
                          (if (>= (length z) (add1 x))
                              z
                              (cons y z)))
                        '()
                        l)))))

; [List-of 1String] -> [List-of [List-of String]]
; produces the list of prefixes for the given list
; of 1strings

(check-expect (suffixes '("a" "b" "c" "d"))
              '(("a")
                ("b" "a")
                ("c" "b" "a")
                ("d" "c" "b" "a")))


(define (suffixes l)
  (build-list (length l)
              (lambda (x)
                 (foldl (lambda (y z) 
                          (if (>= (length z) (add1 x))
                              z
                              (cons y z)))
                        '()
                        l))))
