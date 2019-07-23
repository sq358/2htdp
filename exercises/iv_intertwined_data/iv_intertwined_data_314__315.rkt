;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_314_315) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))


; Exercise 314.

; A [List-of FT] is one of:
; - '()
; - (cons FT [List-of FT])

; [List-of FT] -> Boolean
; For the given list of family tree determines if there
; is a child structure with blue eyes in any of the
; family trees

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)

(define (blue-eyed-child-in-forest? an-fforest)
  (ormap blue-eyed-child? an-fforest))

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
 
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

; Exercise 315.

; [List-of FT] -> Number
; For the given list of family trees produces the avg.
; age of all child structures

(check-expect (average-age-in-forest ff1 2019) 93)
(check-expect (average-age-in-forest ff2 2019) 66.5)

(define (average-age-in-forest an-fforest yr)
  (/ (foldr + 0 (map (lambda (ft) (average-age ft yr)) an-fforest))
     (length an-fforest)))

  
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
