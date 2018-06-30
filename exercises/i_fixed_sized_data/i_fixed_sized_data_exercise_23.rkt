; Exercise 23.
(define (string-first s)
  (substring s 0 1))

; The stepper does not step-into 'substring' function
(string-first "hello world")
