; Exercise 35.
;
; An Input is a String.
;
; String -> 1String
; extracts the last character from a non-empty string.
; given "hello world", expected "d"
; given "cold beer", expected "r"
; given "", expected ""
;
(define (string-last s)
  (if
   (string=? s "") ""
   (substring s (- (string-length s) 1))))

; executed in DrRacket's interaction area
> (string-last "hello world")
"d"
> (string-last "cold beer")
"r"
> (string-last "")
""
