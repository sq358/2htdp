; Exercise 81.

(define-struct since-midnight
  [hours minutes seconds])
; A Sincem is (make-since-midnight Number Number Number)
; interpretation is a point in time since midnight

; SinceMidnight -> Number
; consumes (make-since-midnight h m s) and produces
; the number of seconds that have passed since
; midnight

(check-expect
 (time->seconds
  (make-since-midnight 12 30 2)) 45002)

(define (time->seconds sm)
  (+ (* (since-midnight-hours sm) 3600)
     (* (since-midnight-minutes sm) 60)
     (since-midnight-seconds sm)))
