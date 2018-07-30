; Exercise 62.
(require 2htdp/universe)

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

; A DoorState is one of:
; – LOCKED
; – CLOSED
; – OPEN

; DoorState -> DoorState
; closes an open door over the period of one tick.

(check-expect (door-closer "locked") LOCKED)
(check-expect (door-closer "closed") CLOSED)
(check-expect (door-closer "open") CLOSED)

(define (door-closer state-of-door)
  (cond
    [(equal? state-of-door LOCKED) LOCKED]
    [(equal? state-of-door CLOSED) CLOSED]
    [(equal? state-of-door OPEN) CLOSED]))

; DoorState KeyEvent -> DoorState
; turns key event k onto an action on state s
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)

(define (door-action s k)
  (cond
    [(and (equal? LOCKED s) (string=? "u" k))
     CLOSED]
    [(and (equal? CLOSED s) (string=? "l" k))
     LOCKED]
    [(and (equal? CLOSED s) (string=? " " k))
     OPEN]
    [else s]))

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))
(define (door-render s)
  (text s 40 "red"))

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
    [on-tick door-closer 3]
    [on-key door-action]
    [to-draw door-render]))
