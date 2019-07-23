;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_330__337) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 330.

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define DT.v1
  (cons "read!"
        (cons (cons "part1" (cons "part2" (cons "part3" '())))
              (cons (cons "hang" (cons "draw" '()))
                    (cons (cons "read!" '()) '())))))

; Exercise 331.

; Dir.v1 -> Number
; Computes the number of files in the given directory.v1

(check-expect (how-many.v1 '()) 0)
(check-expect (how-many.v1 DT.v1) 7)

(define (how-many.v1 d)            
  (cond
    [(empty? d) 0]
    [(string? (first d))
     (add1 (how-many.v1 (rest d)))]
    [(cons? (first d))
     (+ (how-many.v1 (first d))
        (how-many.v1 (rest d)))]
    [else
     (error "Invalid directory structure")]))

;; Model v.2

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.


; Exercise 332.

(define DT.v2
  (make-dir
   "TS"
   `("read!" ,(make-dir
               "Text"
               `("part1" "part2" "part3"))
             ,(make-dir
               "Libs"
               `(,(make-dir
                   "Code"
                   `("hang" "draw"))
                 ,(make-dir
                   "Docs" `("read"))))))
  )

; Exercise 333.

; Dir.v2 -> Number
; Computes the number of files in the given directory.v2

(check-expect (how-many.v2 (make-dir "Books" '())) 0)
(check-expect (how-many.v2 DT.v2) 7)

(define (how-many.v2 dt)
  (local (; LOFRD -> Number
          (define (count-in-lofd lofd)
            (cond
              [(empty? lofd) 0]
              [(string? (first lofd))
               (add1 (count-in-lofd (rest lofd)))]
              [(dir? (first lofd))
               (+ (how-many.v2 (first lofd))
                  (count-in-lofd (rest lofd)))]
              [else
               (error "Invalid directory tree")])))
    (if (dir? dt)
        (count-in-lofd (dir-content dt))
        (error "Invalid directory tree"))))

; Exercise 334.

; A Dir.v3 is a structure: 
;   (make-dir String LOFD N Boolean)

;; Model 3

(define-struct file [name size content])

; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; Exercise 335

(define DT.v3
  (make-dir.v3
   "TS"
   `(,(make-dir.v3
       "Text"
       '()
       `(,(make-file "part1" 99 "")
         ,(make-file "part2" 52 "")
         ,(make-file "part3" 17 "")))
     ,(make-dir.v3
       "Libs"
       `(,(make-dir.v3
           "Code"
           '()
           `(,(make-file "hang" 8 "")
             ,(make-file "draw" 2 "")))
         ,(make-dir.v3
           "Docs"
           '()
           `(,(make-file "read!" 19 ""))))
       '()))
   `(,(make-file "read!" 10 ""))))

; Exercise 336

; Dir.v3 -> Number
; Computes the number of files in the given directory.v3

(check-expect (how-many.v3 (make-dir.v3 "Books" '() '())) 0)
(check-expect (how-many.v3 DT.v3) 7)

(define (how-many.v3 dt)
  (local (;Dir* -> N
          (define (count-files-in-dirs dirs)
            (cond
              [(empty? dirs) 0]
              [else
               (+ (how-many.v3 (first dirs))
                  (count-files-in-dirs (rest dirs)))]))
          )
    (+ (length (dir.v3-files dt))
       (count-files-in-dirs (dir.v3-dirs dt)))))

; Exercise 337

; Dir.v4 -> Number
; Computes the number of files in the given directory.v4

(check-expect (how-many.v4 (make-dir.v3 "Books" '() '())) 0)
(check-expect (how-many.v4 DT.v3) 7)

(define (how-many.v4 dt)
  (foldr (lambda (d n) (+ (how-many.v4 d) n))
         (length (dir.v3-files dt))
         (dir.v3-dirs dt)))