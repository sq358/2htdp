; Exercise 82.

(define-struct threelw [f1 f2 f3])
; A threelw is a structure:
;  (make-threelw f1 f2 f3)
;
; word letters consist of the following:
; - 1Strings "a" through "z"
; - #false
;
; interpretation (make-threelw "a" "b" #false) is a three
; letter word.

; threelw threelw -> threelw
; consumes two given three-letter words and produces
; a three-letter word that indicates where the given
; ones agree or disagreee. It retains the contents of
; structure fields if the two agree, otherwise it
; places #false in the field of th resulting word

;; tests
(check-expect
 (compare-word
  (make-threelw "s" "e" "e")
  (make-threelw "s" "e" "e"))
 (make-threelw "s" "e" "e"))

(check-expect
 (compare-word
  (make-threelw "s" "e" "e")
  (make-threelw "s" "e" "a"))
 (make-threelw "s" "e" #false))

(check-expect
 (compare-word
  (make-threelw #false "e" "e")
  (make-threelw #false "e" "a"))
 (make-threelw #false "e" #false))

(define (compare-word w1 w2)
  (cond
    [(equal? w1 w2) w1]
    [else
     (make-threelw
      (if (equal? (threelw-f1 w1) (threelw-f1 w2))
          (threelw-f1 w1) #false)
      (if (equal? (threelw-f2 w1) (threelw-f2 w2))
          (threelw-f2 w1) #false)
      (if (equal? (threelw-f3 w1) (threelw-f3 w2))
          (threelw-f3 w1) #false))]
    ))
