; Exercise 30.
(define BASE-ATTENDEES 120)
(define BASE-PRICE 5.0)
(define ATTENDEES-DIFF 15)
(define CHANGE-IN-PRICE .1)
(define VARIABLE-COST .04)
(define PRICE-SENSETIVITY
   (/ ATTENDEES-DIFF CHANGE-IN-PRICE))

(define (attendees ticket-price)
  (-  BASE-ATTENDEES (* (- ticket-price BASE-PRICE) PRICE-SENSETIVITY)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* VARIABLE-COST (attendees ticket-price)))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

; executed in DrRacket's interaction area
> (profit 1)
691.2
> (profit 2)
1117.2
> (profit 3)
1243.2
> (profit 4)
1069.2
> (profit 5)
595.2
>
