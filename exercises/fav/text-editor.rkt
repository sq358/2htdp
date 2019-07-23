;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname text-editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

; String String -> Editor
; consumes an editor from input pre and post strings

(check-expect
 (create-editor "all" "good")
 (make-editor (reverse (explode "all")) (explode "good")))

(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))

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
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

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
      (remove-first (editor-pre ed))
      (cons (first (editor-pre ed)) (editor-post ed)))]))

; Lo1s -> Lo1s
; prodcues an editor-pre with its first item removed

(check-expect (remove-first '()) '())
(check-expect
 (remove-first (cons "a" (cons "b" (cons "c" '()))))
 (cons "b" (cons "c" '())))

(define (remove-first l)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(empty? (rest l)) '()]
       [else (rest l)])]))

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

; Editor -> Editor
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

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 

(define hello-editor
  (make-editor
   (cons "o" (cons "l" (cons "l" (cons "e" (cons "h" '()))))) '()))

(check-expect
 (editor-render hello-editor)
 (place-image/align
  (beside
   (editor-text (reverse (editor-pre hello-editor)))
   CURSOR
   (editor-text (editor-post hello-editor)))
  1 1
  "left" "top"
  MT))

(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

; Lo1s -> Image
; produces and image from the input list of 1strings

(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))