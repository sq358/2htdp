; Exercise 79.

; A Color is one of:
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

;; examples:
;;  (make-color "white")
;;  (make-color "blue")
;;  (make-color "white")

; H is a Number between 0 and 100.
; interpretation represents a happiness value

;; examples:
;;  (make-h 0)
;;  (make-h 100)
;;  (make-h 40)

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)

;; examples:
;;  (make-person "Ann" "Marry" #false)
;;  (make-person "Harry" "Potter" #true)

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)

;; examples:
;;  (make-dog
;;   (make-person "Harry" "Potter" #true)
;;   "Scotch" 2 50)
;;
;;  (make-dog
;;   (make-person "Ann" "Marry" #false)
;;   "Lilly" 1 100)

; A Weapon is one of:
; — #false
; — Posn
; interpretation #false means the missile hasn't
; been fired yet
; a Posn means it is in flight
;; examples:
;;  (make-weapon (make-posn 30 50))
;;  (make-weapon #false)
