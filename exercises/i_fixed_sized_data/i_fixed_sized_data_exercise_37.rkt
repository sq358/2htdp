; Exercise 37.
;
; An Input is a String.
; String -> String
; procudes a String from input with the first character removed.
;
; given "", expected ""
; given "hello world", expected "ello world"
; given "cold beer", expected "old beer"

(define (string-rest s)
  (if (string=? s "") ""
  (substring s 1)))

; tests
> (string-rest "")
""
> (string-rest "hello world")
"ello world"
> (string-rest "cold beer")
"old beer"
>
