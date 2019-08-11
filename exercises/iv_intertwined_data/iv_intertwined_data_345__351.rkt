;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_345__351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; Exercise 345.

(make-add 20 -10)
(make-add (make-mul 20 3) 33)
(make-add (make-mul 3.14
                    (make-mul 2 3))
          (make-mul 3.14
                    (make-mul -1 -9)))

(+ -1 2)
(+ (* -2 -3) 33)
(* (+ 1 (* 2 3)) 3.14)

; Exercise 346.

; A BSL-expr is one of
; - N
; - add
; - mul
; - [List-of BSL-expr]

; Exercise 347.

; BSL-expr -> N
; Computes the value of the given BSL-expr

(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)

(define (eval-expression v)
  (cond
    [(number? v) v]
    [(add? v) (+ (eval-expression (add-left v))
                 (eval-expression (add-right v)))]
    [(mul? v) (* (eval-expression (mul-left v))
                 (eval-expression (mul-right v)))]))

; Exercise 348

; (define-struct and [conds])
; (define-struct or [conds])
; (define-struct not [expr])

; A Boolean-BS-expr is one of:
; - #true
; - #false
; - &&
; - ||
; - !

; Boolean-BS-expr -> Boolean
; Computes the value of the given Boolean-BS-expr

(define-struct && [left right])
(define-struct || [left right])
(define-struct ! [expr])

(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression (make-! #true)) #false)
(check-expect (eval-bool-expression (make-! (make-&& #false #true))) #true)
(check-expect (eval-bool-expression (make-&& #false #true)) #false)
(check-expect (eval-bool-expression (make-|| #false #true)) #true)

(define (eval-bool-expression v)
  (cond
    [(eq? v #t) v]
    [(eq? v #f) v]
    [(&&? v) (and (eval-bool-expression (&&-left v))
                  (eval-bool-expression (&&-right v)))]
    [(||? v) (or (eval-bool-expression (||-left v))
                 (eval-bool-expression (||-right v)))]
    [(!? v) (not (eval-bool-expression (!-expr v)))]))

; Exercise 349.

(define WRONG "Wrong expression provided")

(check-expect (parse 2) 2)
(check-expect (parse '(+ 2 4)) (make-add 2 4))
(check-expect (parse '(* 4 5)) (make-mul 4 5))
(check-error (parse "hello"))
(check-error (parse 'cond))
(check-error (parse '(/ 4 5)))
(check-error (parse '(add1 3)))
(check-error (parse '(+ 2 3 4)))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(define (atom? s)
  (or (number? s) (string? s) (symbol? s)))

; Exercise 350.

; The function checks for limited number of variables?

; Exercise 351.

; S-expr -> N
; Evaluates the given s-expressions

(check-expect (interpreter-expr '(+ 2 3)) 5)
(check-expect (interpreter-expr '(* 3 2)) 6)
(check-error (interpreter-expr '(add1 2)))

(define (interpreter-expr s)
  (local ((define PARSE-TREE (parse s)))
  (cond
    [(number? PARSE-TREE) PARSE-TREE]
    [(add? PARSE-TREE)
     (+ (interpreter-expr (add-left PARSE-TREE))
        (interpreter-expr (add-right PARSE-TREE)))]
    [(mul? PARSE-TREE)
     (* (interpreter-expr (mul-left PARSE-TREE))
        (interpreter-expr (mul-right PARSE-TREE)))]
    [else
     (error WRONG)])))
           




