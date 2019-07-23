;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_285_290) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 285.

(define EUD-USD 1.06)

(define (convert-euro cs)
  (map (lambda (c) (* c EUD-USD)) cs))

(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p))) lop))

; Exercise 286.

(define-struct ir [name description acq-price sales-price])

(define TEST-LOI
  (list (make-ir "inv1" "inv1" 200 210)
        (make-ir "inv2" "inv2" 180 250)
        (make-ir "inv3" "inv3" 20 60)))

; [List-of InventoryRecord] -> [List-of InventoryRecord]
; Produces a list of inventory records sorted by the
; difference between their acquisition price and sales
; price

; [List-of Inventory] -> [List-of Inventory]
; sorts the list of inventories by the difference
; between two prices in descending order

(check-expect (sort-inv '()) '())
(check-expect (sort-inv TEST-LOI)
              (list (make-ir "inv2" "inv2" 180 250)
                    (make-ir "inv3" "inv3" 20 60)
                    (make-ir "inv1" "inv1" 200 210)))

(define (sort-inv loi)
  (mysort loi
        (lambda (a b) (>= (price-diff a) (price-diff b)))))

(define (mysort loi f)
  (cond
    [(empty? loi) '()]
    [else
     (insert (first loi) (mysort (rest loi) f) f)]))

(define (insert i loi cmp)
  (cond
    [(empty? loi) (list i)]
    [else
     (if (cmp i (first loi))
         (cons i loi)
         (cons (first loi) (insert i (rest loi) cmp)))]))

(define (price-diff i)
  (abs (- (ir-acq-price i) (ir-sales-price i))))

; Exercise 287.

; Number [List-of InventoryItem] -> [List-of InventoryItem]
; Returns all the inventory items that their acq.
; price is less than the given number

(check-expect (eliminate-exp 200 TEST-LOI)
              (list (make-ir "inv2" "inv2" 180 250)
                    (make-ir "inv3" "inv3" 20 60)))

(define (eliminate-exp ua loi)
  (filter (lambda (a) (< (ir-acq-price a) ua)) loi))

; InventoryItem [List-of InventoryItem] -> [List-of Inventory]
; eliminates the given inventory item in the output list of
; inventory items

(check-expect (recall "inv1" '()) '())

(check-expect (recall "inv1" TEST-LOI)
              (list (make-ir "inv2" "inv2" 180 250)
                    (make-ir "inv3" "inv3" 20 60)))

(define (recall ty loi)
  (filter (lambda (a) (not (string=? ty (ir-name a))))
          loi))

; Exercise 288.

; N -> [List-of N]
(check-expect (ex288.1 2) '(0 1))

(define (ex288.1 n)
  (build-list n (lambda (b) b)))

; N -> [List-of N]
(check-expect (ex288.2 2) '(1 2))

(define (ex288.2 n)
  (build-list n (lambda (b) (add1 b))))

; N -> [List-of N]
(check-expect (ex288.3 3) '(1 1/2 1/3))

(define (ex288.3 n)
  (build-list n (lambda (b) (/ 1 (add1 b)))))

; N -> [List-of N]
(check-expect (ex288.4 3) '(0 2 4))

(define (ex288.4 n)
  (build-list n (lambda (b) (* b 2))))

; N -> [List-of [List-of N]]
(check-expect (ex288.5 3)
              (list '(1 0 0) '(0 1 0) '(0 0 1)))

(define (ex288.5 n)
  (build-list n (lambda (a)
                  (build-list n (lambda (b)
                                  (if (= a b) 1 0))))))

; N [N -> N] -> [List-of N]

(check-within (tabulate 2 sqrt)
              (list (sqrt 0) (sqrt 1) (sqrt 2))
              .1)

(define (tabulate n fn)
  (build-list (add1 n) (lambda (a) (fn a))))

; Exercise 289.

; Name [List-of-Name] -> Boolean
; for the given name and list of names, determines if any
; name in the latter is equal or an extention of former

(check-expect (find-name "arash" '()) #false)
(check-expect (find-name "arash" '("bob" "molly"))
              #false)

(check-expect (find-name "arash" '("arash" "molly"))
              #true)

(check-expect (find-name "won" '("book" "wonder" "fact"))
              #true)

(define (find-name name lon)
  (ormap (lambda (n)
           (and (<= (string-length name) (string-length n))
                (string=? (substring n 0 (string-length name)) name)))
         lon))

; Q. Should you use ormap or andmap to define a function that ensures
;    that no name on some list exceeds some given width?
; A. We should 'andmap' because we're interested in having all names
;    meet the condition.

; Exercise 290.

; [List-of-Number] [List-of-Number] -> [List-of-Number]
; for two given list of numbers, appends the second list to the first
; list

(check-expect (append-from-fold '(1 2 3) '()) '(1 2 3))
(check-expect (append-from-fold '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (append-from-fold l1 l2)
   (foldr (lambda (n l) (cons n l)) l2 l1))

; if foldl is used the tests will fail

(define dot1 (circle 4 "solid" "red"))
(define dot2 (circle 4 "solid" "green"))

(check-expect (beside-from-fold (list dot1 dot2))
              (beside dot1 (beside dot2 empty-image)))

(define (beside-from-fold l)
  (foldr (lambda (im1 im2) (beside im1 im2)) empty-image l))


(check-expect (above-from-fold (list dot1 dot2))
              (above dot1 (above dot2 empty-image)))

(define (above-from-fold l)
  (foldr (lambda (im1 im2) (above im1 im2)) empty-image l))

; Exercise 291.

; [List-of-Any -> List-of-Any] [List-of-Any] -> [List-of-Any]
; applies the given functions to the items of the given list
; of items

(check-expect (map-via-fold add1 '()) '())
(check-expect (map-via-fold add1 '(1 2 3)) '(2 3 4))

(define (map-via-fold func l)
  (foldr (lambda (x y) (cons (func x) y)) '() l))



