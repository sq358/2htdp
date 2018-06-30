; Exercise 38.
;
; An Input is a String
; String -> String
; produces a String from Input with the last character removed.
;
; given "", expected ""
; given "hello world!", expected "hello world"
; given "cold beer", "cold bee"
(define (string-remove-last s)
  (if (string=? s "") ""
      (substring s 0 (- (string-length s) 1))))

; tests
> (string-remove-last "")
""
> (string-remove-last "hello world!")
"hello world"
> (string-remove-last "cold beer")
"cold bee"
>
