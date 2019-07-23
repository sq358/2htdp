;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_310_313) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; Exercise 310.

; FT -> Number
; For the given family tree counts the number child
; structure in the tree

(check-expect (count-persons NP) 0)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)

(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ 1 (count-persons (child-father an-ftree))
             (count-persons (child-mother an-ftree)))]))

; Exercise 311.

; FT Number -> Number
; For the given family tree and year calculates the avg.
; age of all child structures in the family tree

(check-expect (average-age NP 2019) 0)
(check-expect (average-age Adam 2019) 85)

(define (average-age an-ftree yr)
  (cond
    [(no-parent? an-ftree) 0]
    [else (/ (sum-of-all-ages yr an-ftree)
             (count-persons an-ftree))]))

(define (sum-of-all-ages yr an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (- yr (child-date an-ftree))
             (sum-of-all-ages yr (child-father an-ftree))
             (sum-of-all-ages yr (child-mother an-ftree)))]))

; Exercise 312.

; FT -> [List-of Color]
; For the given family tree, produces the list of eye
; colors of all the child structures in the family tree

(check-expect (eye-colors NP) '())
(check-expect (eye-colors Adam) '("hazel" "green" "green"))
(check-expect (eye-colors Gustav)
              '("brown" "pink" "blue" "green" "green"))

(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (append (list (child-eyes an-ftree))
                  (eye-colors (child-father an-ftree))
                  (eye-colors (child-mother an-ftree)))]))

; Exercise 313.

; FT -> Boolean
; For the given family tree determines if any ancestor
; in the family tree has blue eyes

;; Q. Explain why this function fails one of its tests.
;; What is the result of (blue-eyed-ancestor? A) no matter
;;  which A you choose?
;; A. The result is always false

; (define (blue-eyed-ancestor? an-ftree)
;  (cond
;    [(no-parent? an-ftree) #false]
;    [else
;     (or
;       (blue-eyed-ancestor?
;         (child-father an-ftree))
;       (blue-eyed-ancestor?
;         (child-mother an-ftree)))]))

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or
      (if (child? (child-father an-ftree))
          (or (string=? (child-eyes (child-father an-ftree)) "blue")
              (blue-eyed-ancestor? (child-father an-ftree)))
          #false)
      (if (child? (child-mother an-ftree))
          (or (string=? (child-eyes (child-mother an-ftree)) "blue")
              (blue-eyed-ancestor? (child-mother an-ftree)))
          #false))]))