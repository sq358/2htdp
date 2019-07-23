;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_170) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 170.

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

; ListOfPhones is one of:
; - '()
; - (cons Phone ListOfPhones)

; ListOfPhones -> ListOfPhones
; For phones in input list-of-phones replaces all
; the occurences of area code 713 with 281

(check-expect (replace '()) '())

(check-expect
 (replace
  (cons (make-phone 233 345 763) (cons (make-phone 342 400 912) '())))
 (cons (make-phone 233 345 763) (cons (make-phone 342 400 912) '())))

(check-expect
 (replace
  (cons (make-phone 233 345 763) (cons (make-phone 713 400 912) '())))
 (cons (make-phone 233 345 763) (cons (make-phone 281 400 912) '())))

(define (replace lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cond
       [(= (phone-area (first lop)) 713)
        (cons
         (make-phone 281 (phone-switch (first lop)) (phone-four (first lop)))
         (replace (rest lop)))]
       [else (cons (first lop) (replace (rest lop)))])]))