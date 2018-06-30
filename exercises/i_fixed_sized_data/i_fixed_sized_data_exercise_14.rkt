; Exercise 14.
(define (string-last s)
  (if (= (string-length s) 0) ""
      (substring s 0 (- (string-length s) 1))))
