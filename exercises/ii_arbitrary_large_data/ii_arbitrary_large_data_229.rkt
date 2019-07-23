;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_229) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 229.

(require 2htdp/universe)
 
; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)
 
(define COLORS
  (list "white" "yellow"))

(define fsm-exercise-109
  (list
   (make-ktransition "white" "a" "yellow")
   (make-ktransition "yellow" "b" "yellow")
   (make-ktransition "yellow" "c" "yellow")))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))
  
; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
                (make-fs fsm-exercise-109 "yellow"))
              (square 100 "solid" "yellow"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs

(check-expect
  (find-next-state (make-fs fsm-exercise-109 "white") "a")
  (make-fs fsm-exercise-109 "yellow"))
(check-expect
  (find-next-state (make-fs fsm-exercise-109 "yellow") "b")
  (make-fs fsm-exercise-109 "yellow"))
(check-expect
  (find-next-state (make-fs fsm-exercise-109 "yellow") "c")
  (make-fs fsm-exercise-109 "yellow"))

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing the current in transitions 
(check-expect (find fsm-exercise-109 "white" "a") "yellow")
(check-expect (find fsm-exercise-109 "yellow" "b") "yellow")
(check-expect (find fsm-exercise-109 "yellow" "c") "yellow")

(define (find transitions current ke)
 (cond
   [(empty? transitions) (error "transitions cannot be empty")]
   [else
    (if (and
         (state=? (ktransition-current (first transitions)) current)
         (string=? (ktransition-key (first transitions)) ke))
        (ktransition-next (first transitions))
        (if (empty? (rest transitions))
            (error "not found: " current)
            (find (rest transitions) current ke)))]))

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