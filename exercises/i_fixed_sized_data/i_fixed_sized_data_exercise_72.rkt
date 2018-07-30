; Exercise 72.

; a Phone is a structure
;    (make-phone (Number String))
; interpretation (make-phone area number) is a phone
; number.
; area area-code
; number phone number


;; For (define-struct phone# [area switch num])

;; a Phone# is a structure
;    (make-phone (Number Number Number))
; interpretation (make-phone area switch num) is a
; phone number.
;
; area is a number >= 100 and <= 999
; switch is a number >= 100 and < 999
; num is a number >= 1000 and <= 9999
