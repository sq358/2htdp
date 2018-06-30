; Exercise 34.

; An Input is a String
;
; String -> 1String
; extracts the first character from a non-empty string.
; the assumption is that the input is always non-empty.
;
; given "hello world!", expected "h"
; given "cold beer", expected "c"
(define (string-first s)
  (substring s 0 1))

> (string-first "hello world")
"h"
> (string-first "cold beer")
"c"
> (string-last "")
""
