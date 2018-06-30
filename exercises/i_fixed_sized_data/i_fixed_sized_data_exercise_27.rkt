; Exercise 27.
(define BASE-ATTENDEES 120)
(define BASE-PRICE 5.0)
(define ATTENDEES-DIFF 15)
(define CHANGE-IN-PRICE .1)
(define FIXED-COST 180)
(define VARIABLE-COST .04)

(define (attendees ticket-price)
  (-  BASE-ATTENDEES (* (- ticket-price BASE-PRICE) (/ ATTENDEES-DIFF CHANGE-IN-PRICE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST (* VARIABLE-COST (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

; executed in DrRacket's interaction area
> (profit 1)
511.2
> (profit 2)
937.2
> (profit 3)
1063.2
> (profit 4)
889.2
> (profit 5)
415.2
>
