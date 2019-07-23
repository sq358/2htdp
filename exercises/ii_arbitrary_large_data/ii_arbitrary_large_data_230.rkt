;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_230) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 230.

(require 2htdp/universe)
 
(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)
 
; FSM-State is a Color.
 
(define COLORS
  (list "green" "red" "yellow" "white"))

(define ERR "red")

(define fsm-exercise-109
  (make-fsm
   "white"
   (list
    (make-transition "white" "a" "yellow")
    (make-transition "yellow" "b" "yellow")
    (make-transition "yellow" "c" "yellow")
    (make-transition "yellow" "d" "green")
    (make-transition "white" "not a" "red")
    (make-transition "yellow" "not b, c, or d" "red"))
   "green"))

(define fsm-exercise-109-yellow
  (make-fsm
   "yellow"
   (list
    (make-transition "white" "a" "yellow")
    (make-transition "yellow" "b" "yellow")
    (make-transition "yellow" "c" "yellow")
    (make-transition "yellow" "d" "green")
    (make-transition "white" "not a" "red")
    (make-transition "yellow" "not b, c, or d" "red"))
   "green"))

(define fsm-exercise-109-green
  (make-fsm
   "green"
   (list
    (make-transition "white" "a" "yellow")
    (make-transition "yellow" "b" "yellow")
    (make-transition "yellow" "c" "yellow")
    (make-transition "yellow" "d" "green")
    (make-transition "white" "not a" "red")
    (make-transition "yellow" "not b, c, or d" "red"))
   "green"))

; FSM -> FSM
; match the keys pressed with the given FSM 
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when stop? state-as-colored-square]))
  
; FSM -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square fsm-exercise-109)
              (square 100 "solid" "white"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))
 
; FSM KeyEvent -> FSM
; finds the next state from ke and cs

(check-expect
  (find-next-state fsm-exercise-109 "a")
  (make-fsm
   "yellow"
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(check-expect
  (find-next-state fsm-exercise-109-yellow "b")
  (make-fsm
   "yellow"
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(check-expect
  (find-next-state fsm-exercise-109-yellow "c")
  (make-fsm
   "yellow"
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(check-expect
  (find-next-state fsm-exercise-109-yellow "d")
  (make-fsm
   "green"
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(check-expect
  (find-next-state fsm-exercise-109 "d")
  (make-fsm
   ERR
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(check-expect
  (find-next-state fsm-exercise-109 "h")
  (make-fsm
   ERR
   (fsm-transitions fsm-exercise-109)
   (fsm-final fsm-exercise-109)))

(define (find-next-state an-fsm ke)
  (make-fsm (find an-fsm ke)
            (fsm-transitions an-fsm)
            (fsm-final an-fsm)))

; FSM KeyEvent -> FSM-State
; finds the state representing the current in transitions 
(check-expect (find fsm-exercise-109 "a") "yellow")
(check-expect (find fsm-exercise-109 "b") ERR)
(check-expect (find fsm-exercise-109 "c") ERR)

(define (find an-fsm ke)
  (find-transition (fsm-transitions an-fsm) (fsm-initial an-fsm) ke))

(define (find-transition transitions initial ke)
 (cond
   [(empty? transitions) ERR]
   [else
    (if (and (state=? initial (transition-current (first transitions)))
             (string=? (transition-key (first transitions)) ke))
        (transition-next (first transitions))
        (if (empty? (rest transitions))
            ERR
            (find-transition (rest transitions) initial ke)))]))

; Any -> Boolean
; determines if the arbitrary input is an FSM-State or not

(check-expect (state=? "white" "white") #true)
(check-expect (state=? "yellow" "white") #false)
(check-error (state=? "hello" "red"))
(check-error (state=? "blue" 1))
(check-error (state=? #true 100))

(define (state=? s1 s2)
  (cond
    [(and (member s1 COLORS) (member s2 COLORS))
     (if (string=? s1 s2) #true #false)]
    [(member s2 COLORS)
     (error "state=?: expects a FSM-State as 1st argument, given " s1)]
    [(member s1 COLORS)
     (error "state=?: expects a FSM-State as 2nd argument, given " s2)]
    [else
     (error
      "state=?: expects FSM-State for its arguments, given " s1 " and " s2)]))

; FSM -> Boolean
; determines if the FSM is in its final state

(check-expect (stop? fsm-exercise-109) #false)
(check-expect (stop? fsm-exercise-109-green) #true)

(define (stop? an-fsm)
  (state=? (fsm-initial an-fsm) "green"))