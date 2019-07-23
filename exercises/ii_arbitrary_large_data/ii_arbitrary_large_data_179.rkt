;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_179) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 179.

(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; String String -> Editor
; consumes an editor from input pre and post strings

(check-expect
 (create-editor "all" "good")
 (make-editor (rev (explode "all")) (explode "good")))

(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
 
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

; insert the 1String k between pre and post

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible

(check-expect
  (editor-lft
   (make-editor '() '()))
  (make-editor '() '()))

(check-expect
  (editor-lft
   (make-editor (cons "c" '()) (cons "d" (cons "e" (cons "f" '())))))
   (make-editor '() (cons "c" (cons "d" (cons "e" (cons "f" '()))))))

(check-expect
  (editor-lft
   (make-editor '() (cons "c" (cons "d" (cons "e" (cons "f" '()))))))
  (make-editor '() (cons "c" (cons "d" (cons "e" (cons "f" '()))))))

(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor
      (remove-last (editor-pre ed))
      (cons (last (editor-pre ed)) (editor-post ed)))]))

; Lo1s -> Lo1s
; prodcues an editor-pre with its last item removed

(check-expect (remove-last '()) '())
(check-expect
 (remove-last (cons "a" (cons "b" (cons "c" '()))))
 (cons "a" (cons "b" '())))

(define (remove-last l)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(empty? (rest l)) '()]
       [else (cons (first l) (remove-last (rest l)))])]))

; Lo1s -> 1String
; returns the last 1String item

(check-expect (last '()) '())
(check-expect
 (last (cons "a" (cons "b" (cons "c" '()))))
 "c")

(define (last l)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(empty? (rest l)) (first l)]
       [else (last (rest l))])]))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible

(check-expect
  (editor-rgt
   (make-editor '() '()))
  (make-editor '() '()))

(check-expect
  (editor-rgt
   (make-editor
    (cons "c" '()) (cons "d" (cons "e" (cons "f" '())))))
   (make-editor
    (cons "d" (cons "c" '())) (cons "e" (cons "f" '()))))

(check-expect
  (editor-rgt
   (make-editor
    (cons "f" (cons "e" (cons "d" (cons "c" '())))) '()))
   (make-editor
    (cons "f" (cons "e" (cons "d" (cons "c" '())))) '()))

(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
     (make-editor
      (cons (first (editor-post ed)) (editor-pre ed))
      (rest (editor-post ed)))]))

; Editor -> Editor
; deletes a 1String to the left of the cursor
; if possible


(check-expect
  (editor-del
   (make-editor '() '()))
  (make-editor '() '()))

(check-expect
 (editor-del
  (make-editor
   (cons "c" '()) (cons "d" (cons "e" (cons "f" '())))))
 (make-editor
  '() (cons "d" (cons "e" (cons "f" '())))))

(check-expect
 (editor-del
  (make-editor '() (cons "d" (cons "e" (cons "f" '())))))
 (make-editor '() (cons "d" (cons "e" (cons "f" '())))))

(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor
      (rest (editor-pre ed)) (editor-post ed))])) 
  

 