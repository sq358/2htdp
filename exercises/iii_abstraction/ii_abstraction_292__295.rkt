;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_292__293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 292.

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp lox)
  (cond
    [(empty? (rest lox)) #true]
    [else
     (and
      (cmp (first lox) (second lox))
      (sorted? cmp (rest lox)))]))

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean 
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(define (sorted.v1 cmp)
  (lambda (l0)
    (if (empty? l0) #true (sorted? l0 cmp))))

; Q. Could you redefine sorted to use sorted? from exercise 292?
; A. Yes, we can do this by removing local and reference sorted?

; Explain why sorted/l does not consume cmp as an argument.
; A. Because sorted/l is defined in local context, therefore it
; already has access to cmp.

; Exercise 293.

; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise

(define a-random-num (random 100))
(define a-random-list (build-list 1000 (lambda (n) (random (add1 n)))))

(check-satisfied (find a-random-num a-random-list)
                 (found? a-random-num a-random-list))

(check-satisfied (find 2 '()) (found? 2 '()))

(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; X [List-of X] -> [[List-of X] -> Boolean]

(define (found? n alist)
  (lambda (v)
    (cond
      [(equal? v #false)
       (if (member n alist) #false #true)]
      [(list? v)
       (cond
         [(> (length v) (length alist)) #false]
         [(not (member n v)) #false]
         [else (equal? (sublist n alist) v)])]
      [else #false])))

; X [List-of X] -> [List-of X]
; extracts the sublist of given list starting from the index
; of the given x.
; if x doesn't exist in the given list, returns an empty list

(check-expect (sublist 1 '(2 3 4)) '())
(check-expect (sublist 1 '()) '())
(check-expect (sublist 2 '(1 2 3 4)) '(2 3 4))

(define (sublist n alist)
  (cond
    [(empty? alist) '()]
    [else
     (if (equal? n (first alist)) alist
         (sublist n (rest alist)))]))

; Exercise 294.

; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise

(check-satisfied (index 2 '(1 2 3)) (is-index? 2 '(1 2 3)))

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))


; X [List-of X] -> [X -> Boolean]
; returns a specification function for index

(define (is-index? x l)
  (lambda (v)
    (cond
      [(equal? v #false)
       (if (member x l) #false #true)]
      [(number? v)
       (cond
         [(> v (- (length l) 1)) #false]
         [else
          (= (varient-of-index x l) v)])]
      [else #false])))

; X [List-of X] -> [Maybe X]
; determines the index of the first occurance
; of x in l, #false otherwise

(define (varient-of-index x l)
  (local (; counts the current index of the list
          (define (count-index n alist)
            (cond
              [(empty? alist) -1]
              [else
               (if (equal? (first alist) x) n
                (count-index (add1 n) (rest alist)))])))
    (count-index 0 l)))

; Exercise 295.

; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
(define (random-posns/bad n)
  (build-list
    (add1 n)
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))
  
    
; Number -> [[List-of-Posn -> Boolean]]
; ensures that the length of the given list is some given
; count and all the posns in this list are within a WIDTH
; by HEIGHT rectangle

(define (n-inside-playground? n)
  (lambda (lop)
    (cond
      [(empty? lop)
       (if (= n 0) #true #false)]
      [(list? lop)
       (is-valid? lop)]
      [else #false])))

(define (is-valid? lop)
  (cond
    [(empty? lop) #false]
    [else
     (andmap (lambda (p)
               (cond
                 [(posn? p)
                  (and
                   (and (>= (posn-x p) 0) (< (posn-x p) WIDTH))
                   (and (>= (posn-y p) 0) (< (posn-y p) HEIGHT)))]
                 [else
                  #false])) lop)]))