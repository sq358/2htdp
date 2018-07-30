(define-struct threelw [f1 f2 f3])
; A threelw is a structure:
;  (make-threelw f1 f2 f3)
;
; word letters consist of the following:
; - 1Strings "a" through "z"
; - #false
;
; interpretation (make-threelw "a" "b" #false) is a three
; letter word.
