;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_114_3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 114_3.

(require 2htdp/universe)

(define MTSN
  (empty-scene 200 20))

(define CURSOR
  (rectangle 1 20 "solid" "red"))

(define TXT-SIZE 11)
(define TXT-COLOR "black")

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

; Given the pre field of an editor, it launches
; an interactive editor
(define (run pre)
  (big-bang (make-editor pre "")
   [to-draw render]
   [on-key edit]
   [check-with is-editor?]))

(define (is-editor? e)
  (cond
    [(and
      (string=? (editor-pre e)) (string=? (editor-post e)))
     #true]
    [else #false]))

; Editor -> Image
; Renders the text within an MTSN.

;; tests
(check-expect
 (render (make-editor "hello" "world"))
         (overlay/align "left" "center"
                        (beside
                         (editor-text "hello")
                         CURSOR
                         (editor-text "world"))
                        MTSN))

(define (render e)
  (overlay/align "left" "center"
                 (beside
                  (editor-text (editor-pre e))
                  CURSOR
                  (editor-text (editor-post e)))
                 MTSN))

;; tests
(check-expect (editor-text "hello")
     (text "hello" TXT-SIZE TXT-COLOR))

(define (editor-text s)
  (text s TXT-SIZE TXT-COLOR))

; Editor KeyEvent -> Editor
; adds a single-character KeyEvent ke to the end of
; (editor-pre ed), unless KeyEvent ke denotes ("\b")
; in that case, it deletes the character immediately
; to the left of the cursor (if there any).
; Function ignores ("\t") and ("\r") KeyEvents
;
; Function pays attention to only two KeyEvents longer
; than one character: "left" and "right".
; The left arrow moves the cursor one character to the
; left (if any), and the right arrow moves it one
; character to the right (if any).
; All other such KeyEvents are ignored.

(check-expect
 (edit (make-editor "hello" "world") " ")
 (make-editor "hello " "world"))

(check-expect
 (edit (make-editor "hello " "world") "\b")
 (make-editor "hello" "world"))

(check-expect
 (edit (make-editor "hello" "world") "\t")
 (make-editor "hello" "world"))

(check-expect
 (edit (make-editor "hello" "world") "\r")
 (make-editor "hello" "world"))

(check-expect
 (edit (make-editor "hello" "world") "left")
 (make-editor "hell" "oworld"))

(check-expect
 (edit (make-editor "hello" "world") "right")
 (make-editor "hellow" "orld"))

(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (if (string=? ke "\b")
         (make-editor
          (string-remove-last (editor-pre ed))
          (editor-post ed))
         (if (and (not (string=? ke "\t")) (not (string=? ke "\r")))
             (make-editor
              (string-append (editor-pre ed) ke)
              (editor-post ed))
             ed))]
    [(string=? ke "left")
     (make-editor
      (string-remove-last (editor-pre ed))
      (string-append
       (string-last (editor-pre ed))
       (editor-post ed)))]
    [(string=? ke "right")
     (make-editor
      (string-append
       (editor-pre ed) (string-first (editor-post ed)))
       (string-rest (editor-post ed) 1))]
    ))

;; tests
(check-expect
 (string-remove-last "hello") "hell")

(define (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))

;; tests
(check-expect
 (string-last "hello") "o")

(define (string-last s)
  (substring
   s (- (string-length s) 1) (string-length s)))

;; tests
(check-expect
 (string-first "hello") "h")

(define (string-first s)
  (substring s 0 1))

;; tests
(check-expect
 (string-rest "hello" 2) "llo")

(define (string-rest s i)
  (substring s i (string-length s)))