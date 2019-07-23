;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_190) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 190.

; Prefix is one of:
; - '()
; - (cons 1String Prefix)

; ListOfPrefixes is one of:
; - '()
; - (cons Prefix ListOfPrefixes)

; ListOfSuffixes is one of:
; - '()
; - (cons Suffix ListOfSuffixes)

; ListOf1Strings -> ListofPrefixes
; for list of 1strings produces the list of all prefixes

(check-expect (prefixes '()) '())

(check-expect
 (prefixes (list "a")) (list (list "a")))

(check-expect
 (prefixes (list "a" "b"))
 (list
  (list "a" "b") (list "a")))

(check-expect
 (prefixes (list "a" "b" "c" "d"))
 (list
  (list "a" "b" "c" "d")
  (list "a" "b" "c")
  (list "a" "b")
  (list "a")))

(define (prefixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (insert.pfx (first lo1s) (prefixes (rest lo1s)))]))

; 1String ListPrefixes -> ListOfPrefixes
; produces a list of prefixes by inserting 1string into input
; list of prefixes

(check-expect (insert.pfx "c" '()) (list (list "c")))

(check-expect
 (insert.pfx "b" (list (list "c")))
 (list (list "b" "c") (list "b")))

(check-expect
 (insert.pfx "a" (list (list "b") (list "b" "c")))
 (list
  (list "a" "b") (list "a" "b" "c") (list "a")))

(check-expect
 (insert.pfx "a" (list (list "b") (list "b" "c") (list "b" "c" "d")))
 (list
  (list "a" "b") (list "a" "b" "c") (list "a" "b" "c" "d") (list "a")))

(define (insert.pfx 1s lop)
  (cond
    [(empty? lop) (list (list 1s))]
    [(cons? lop)
     (cons
      (cons 1s (first lop))
      (insert.pfx 1s (rest lop)))]))

; ListOf1Strings -> ListOfSuffixes
; produces the list of suffixes from the input list of 1strings

(check-expect (suffixes '()) '())
(check-expect
 (suffixes (list "a"))
 (list (list "a")))

(check-expect
 (suffixes (list "a" "b"))
 (list (list "a" "b") (list "b")))

(check-expect
 (suffixes (list "a" "b" "c"))
 (list (list "a" "b" "c") (list "b" "c") (list "c")))

(define (suffixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
      (cons
       lo1s (suffixes (rest lo1s)))]))