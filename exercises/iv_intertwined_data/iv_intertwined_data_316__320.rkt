;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_316__320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
	
; An Atom is one of: 
; – Number
; – String
; – Symbol 

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; Exercise 316.

; Any -> Boolean
; For the given input determines if it's an atom or not

(check-expect (atom? 2) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'yum) #true)
(check-expect (atom? (lambda (d) #true)) #false)

(define (atom? a)
  (or (string? a)
      (number? a)
      (symbol? a)))

; Exercise 317.

; S-expr Symbol -> N 
; Counts all occurrences of sy in sexp

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

(define (count sexp sy)
  (local (; SL -> N 
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))
          ; Atom -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))

; Exercise 318.

; S-expr -> Number
; Returns the depth of the give s-expression

(check-expect (depth 2) 1)
(check-expect (depth '(("hello") 2 ("my" "world"))) 3)
(check-expect (depth '(
                       ("hello" (1))
                       "tree"
                       ("brave" "new" "world"))
                     )
              4)

(define (depth sexp)
  (local (; SL -> Number
          ; returns 
          (define (depth-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (max (depth (first sl))
                    (depth-sl (rest sl)))])))
    (cond
      [(atom? sexp) 1]
      [else
       (add1 (depth-sl sexp))])))

; Exercise 319.

; S-expr Symbol Symbol -> S-expr
; With the given s-expr s and old and new symbols
; returns a new s-expr with old symbols replaced
; with new

(check-expect (substitute 'hello 'hello 'world) 'world)
(check-expect (substitute '((hello world) (brave "new" world))
                          'world
                          'universe)
              '((hello universe) (brave "new" universe)))

(define (substitute s sy-old sy-new)
  (local (; SL -> SL
          ; For the given SL sl replaces sy-old with sy-new
          (define (substitute-sl sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons (substitute (first sl) sy-old sy-new)
                     (substitute-sl (rest sl)))]))
          ; Atom -> Atom
          ; For the given atom, replaces sy-old with sy-new
          (define (substitute-atom at)
            (cond
              [(symbol? at)
               (if (symbol=? at sy-old)
                   sy-new
                   at)]
              [else at])))
    (cond
      [(atom? s) (substitute-atom s)]
      [else
       (substitute-sl s)])))

; Exercise 320.

; An S-expr is one of: 
; – Number
; - String
; - Symbol
; - [List-of S-expr]

; S-expr Symbol -> N 
; Counts all occurrences of sy in sexp

(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)

(define (count.v2 sexpr sy)
  (cond
    [(empty? sexpr) 0]
    [(number? sexpr) 0]
    [(string? sexpr) 0]
    [(symbol? sexpr) (if (symbol=? sexpr sy) 1 0)]
    [else
     (+ (count.v2 (first sexpr) sy)
        (count.v2 (rest sexpr) sy))]))

; An S-expr is one of:
; - '()
; – Atom
; - (cons S-expr S-expr)

; S-expr Symbol -> N 
; Counts all occurrences of sy in sexp

(check-expect (count.v3 'world 'hello) 0)
(check-expect (count.v3 '(world hello) 'hello) 1)
(check-expect (count.v3 '(((world) hello) hello) 'hello) 2)

(define (count.v3 sexpr sy)
  (cond
    [(atom? sexpr)
     (if (symbol=? sexpr sy) 1 0)]
    [else
     (+ (count.v3 (first sexpr) sy)
        ((lambda (sl)
           (cond
             [(empty? sl) 0]
             [else (count.v3 sl sy)])) (rest sexpr)))]))


; Exercise 321.

; An S-expr is one of: 
; – Atom
; – '()
; - (cons Atom

; An SL is one of: 
; – '()
; – (cons S-expr SL)




                          