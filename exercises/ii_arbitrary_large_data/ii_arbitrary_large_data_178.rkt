;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_178) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 178.

(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

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


; Editor KeyEvent -> Editor
; deals with a key event, given some editor

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))

(check-expect
  (editor-kh (create-editor "cd" "fgh") "\b")
  (create-editor "c" "fgh"))

(check-expect
  (editor-kh (create-editor "cd" "fgh") "left")
  (create-editor "c" "dfgh"))

(check-expect
  (editor-kh (create-editor "cd" "fgh") "right")
  (create-editor "cdf" "gh"))

(check-expect
  (editor-kh (create-editor "" "cdfgh") "left")
  (create-editor "" "cdfgh"))

(check-expect
  (editor-kh (create-editor "cdfgh" "") "right")
  (create-editor "cdfgh" ""))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") ...]
    [(key=? k "right") ...]
    [(key=? k "\b") ...]
    [(key=? k "\t") ...]
    [(key=? k "\r") ...]
    [(= (string-length k) 1) ...]
    [else ...]))

; Explain why the template for editor-kh deals with "\t" and "\r"
; before it checks for strings of length 1.

; Answer: because (string-length "\t") or (string-length "\r") are
; also 1 so if we check for (= (string-length k) 1) before checking
; for (key=? k "\t") and (key=? k "\r") we never reach to the latter
; conditions.





