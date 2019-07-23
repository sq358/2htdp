;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_205) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 205, 206, 207, 208

(require 2htdp/batch-io)
(require 2htdp/itunes)

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)

; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))

; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; LoSA (ListOfStringBooleans):
; - '()
; - (cons StringBoolean LoSA)

(define-struct string-boolean [string boolean])
; A StringBoolean is a structure:
;  (make-string-boolean String Boolean)
; interpretation:
;  (make-string-boolean "time" #false)

;; examples
(define association11
  (cons "album" (cons "sample-album1" '())))

(define association12
  (cons "time" (cons 345 '())))

(define association13
  (cons "artist" (cons #false '())))

(define association14
  (cons "track#" (cons 35 '())))

(define association15
  (cons "play#" (cons #false '())))

(define association21
  (cons "album" (cons "sample-album2" '())))

(define association22
  (cons "time" (cons 1000 '())))

(define association23
  (cons "artist" (cons "sample-artist2" '())))

(define association24
  (cons "track#" (cons #false '())))

(define association25
  (cons "play#" (cons #false '())))

(define lassoc0 '())
(define lassoc1
  (list
   association11
   association12
   association13
   association14
   association15))
(define lassoc2
  (list
   association21
   association22
   association23
   association24
   association25))

(define llists0 '())
(define llists1 (list lassoc1 lassoc2))

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; String LAssoc Any -> Association
; produces the first association whose first item is equal to
; given key (String) or default (Any) if there is not such
; association

(check-expect (find-association "test" '() #false) #false)
(check-expect
 (find-association "date" lassoc2 "not found")
 "not found")
(check-expect
 (find-association "album" lassoc1 "not found")
 association11)

(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else
     (if (string=? key (first (first la)))
         (first la)
         (find-association key (rest la) default))]))

; LLists -> Number
; produces the total amount of play time

(check-expect (total-time/list llists0) 0)
(check-expect (total-time/list llists1) (+ 345 1000))

(define (total-time/list lls)
  (cond
    [(empty? lls) 0]
    [else
     (+
      (second (find-association "time" (first lls) 0))
      (total-time/list (rest lls)))]))

; LLists -> LoSA
; produces strings that are associated with the boolean

(check-expect (boolean-attributes '()) '())
(check-expect
 (boolean-attributes llists1)
 (list
  (make-string-boolean "artist" #false)
  (make-string-boolean "track#" #false)
  (make-string-boolean "play#" #false)))

(define (boolean-attributes lls)
  (cond
    [(empty? lls) '()]
    [else
      (merge
       (create-string-attributes (first lls))
       (boolean-attributes (rest lls)))]))

; LoSA LoSA -> LoSA
; merges the given lists of attributes into a unique

(check-expect (merge '() '()) '())
(check-expect
 (merge
  '()
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false)))
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false)))
(check-expect
 (merge
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false))
  '())
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false)))
(check-expect
 (merge
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false))
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "track#" #false)))
  (list
   (make-string-boolean "artist" #false)
   (make-string-boolean "play#" #false)
   (make-string-boolean "track#" #false)))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else
     (create-set
      (cons
       (first l1)
       (cons
        (first l2) (merge (rest l1) (rest l2)))))]))
 
; LAssoc -> ListOfStringAttributes

(check-expect (create-string-attributes '()) '())
(check-expect
 (create-string-attributes lassoc1)
 (list
  (make-string-boolean "artist" #false)
  (make-string-boolean "play#" #false)))

(define (create-string-attributes la)
  (cond
    [(empty? la) '()]
    [else
     (if (boolean? (second (first la)))
         (cons
          (make-string-boolean
           (first (first la)) (second (first la)))
          (create-string-attributes (rest la)))
         (create-string-attributes (rest la)))]))

(check-expect (create-set '()) '())
(check-expect (create-set (list "a" "b"))
 (list "a" "b"))
(check-expect
 (create-set (list "a" "b" "a" "c" "b" "d"))
 (list "a" "c" "b" "d"))

(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (if (member (first los) (rest los))
         (create-set (rest los))
         (cons
          (first los)
          (create-set (rest los))))]))