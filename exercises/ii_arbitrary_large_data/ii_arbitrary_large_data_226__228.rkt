;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_227) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 226, 227, 228

(require 2htdp/universe)
 
; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)
 
(define COLORS
  (list "white" "yellow" "orange" "green" "red" "blue" "black"))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs

(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))

(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black")
             "not found: black")

(define (find transitions current)
 (cond
   [(empty? transitions) '()]
   [else
    (if (state=? (transition-current (first transitions)) current)
        (transition-next (first transitions))
        (if (empty? (rest transitions))
            (error "not found: " current)
            (find (rest transitions) current)))]))

; Any -> Boolean
; determines if the arbitrary input is an FSM-State or not

(check-expect (state=? "white" "white") #true)
(check-expect (state=? "red" "blue") #false)
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