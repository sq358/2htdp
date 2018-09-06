;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_133) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 133.

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names

;tests
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '()))
              #false)
(check-expect (contains-flatt? (cons "Flatt" '()))
              #true)

(check-expect
 (contains-flatt?
  (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)

(check-expect
 (contains-flatt?
  (cons "A" (cons "Hello" (cons "V" '()))))
 #false)

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (cond
       [(string=? (first alon) "Flatt") #true]
       [else (contains-flatt? (rest alon))])]))

;; 'or' version is better because it's simpler 

(define TEST-LIST
  (cons "Fagan"
  (cons "Findler"
    (cons "Fisler"
      (cons "Flanagan"
        (cons "Flatt"
          (cons "Felleisen"
            (cons "Friedman" '()))))))))

(contains-flatt? TEST-LIST)


