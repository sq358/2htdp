; Excersie 28.
(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

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
