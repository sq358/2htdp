;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_134) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 134.

; List-of-names String -> Boolean
; determines whether string s is on a-list-of-names

;tests
(check-expect (contains? "Moon" '()) #false)
(check-expect (contains? "Find" (cons "Find" '()))
              #true)
(check-expect (contains? "Sam" (cons "Flatt" '()))
              #false)

(check-expect
 (contains? "C"
  (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)

(check-expect
 (contains? "B"
  (cons "A" (cons "Hello" (cons "V" '()))))
 #false)

(define (contains? s alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or
      (string=? s (first alon))
      (contains? s (rest alon)))]))

;; 'or' version is better because it's simpler 

(define TEST-LIST
  (cons "Fagan"
  (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen"
            (cons "Friedman" '()))))))))


