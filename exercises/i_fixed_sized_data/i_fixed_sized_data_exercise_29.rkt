; Exercise 29.
(define BASE-ATTENDEES 120)
(define BASE-PRICE 5.0)
(define ATTENDEES-DIFF 15)
(define CHANGE-IN-PRICE .1)
(define VARIABLE-COST .04)

(define (attendees ticket-price)
  (-  BASE-ATTENDEES (* (- ticket-price BASE-PRICE) (/ ATTENDEES-DIFF CHANGE-IN-PRICE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* VARIABLE-COST (attendees ticket-price)))

(define (profit-composition ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price))))))

; executed in DrRacket's interaction area
> (profit-composition 3)
1243.2
> (profit-composition 4)
1069.2
> (profit-composition 5)
595.2
> (profit 3)
1243.2
> (profit 4)
1069.2
> (profit 5)
595.2
>
