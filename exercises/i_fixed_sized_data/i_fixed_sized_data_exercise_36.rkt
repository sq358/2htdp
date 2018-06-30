; Excercise 36.
;
; An Input is an Image
;
; Image -> Number
; counts the number of pixels in an image img.
;
; given:
;  (empty-scene 100 100) for img
; expected:
;  10000

(define (image-area img)
  (* (image-width img) (image-height img)))

; Tests
> (image-area (empty-scene 100 100))
10000
> (image-area (empty-scene 10 100))
1000

