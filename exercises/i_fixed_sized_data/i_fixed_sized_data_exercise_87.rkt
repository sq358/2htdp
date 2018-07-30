; Exercise 87.

(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)

(define MTSN
  (empty-scene WIDTH HEIGHT))

(define TEXT-LIMIT 64)

(define CURSOR
  (rectangle 1 20 "solid" "red"))

(define TXT-SIZE 11)
(define TXT-COLOR "black")

(define-struct editor [text index])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s i) describes an editor
; whose visible text is s with
; the cursor displayed between (substring s 0 1) and
; CURSOR

; Given the pre field of an editor, it launches
; an interactive editor
(define (run txt)
  (big-bang (make-editor txt (string-length txt))
    [to-draw render]
    [on-key edit]))

; Editor -> Image
; Renders the text within an MTSN.

;; tests
(check-expect
 (render
  (make-editor "hello world" 5))
 (overlay/align "left" "center"
                (overlay/xy
                 (render-text
                  (editor-text (make-editor "hello world" 5)))
                 (image-width
                  (render-text
                   (string-remove-rest "hello world" 5)))
                 0
                 CURSOR)
                 MTSN))

(define (render e)
  (overlay/align "left" "center"
                 (overlay/xy
                  (render-text (editor-text e))
                 (image-width
                  (render-text
                   (string-remove-rest
                    (editor-text e)
                    (editor-index e)))
                  )
                 0
                 CURSOR)
                 MTSN))

;; tests
(check-expect (render-text "hello")
     (text "hello" TXT-SIZE TXT-COLOR))

(define (render-text s)
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
 (edit (make-editor "hello world" 5) "\b")
 (make-editor "hell world" 4))

(check-expect
 (edit (make-editor "hello world" 7) "\t")
 (make-editor "hello world" 7))

(check-expect
 (edit (make-editor "hello world" 7) "\r")
 (make-editor "hello world" 7))

(check-expect
 (edit (make-editor "hello world" 11) "!")
 (make-editor "hello world!" 12))

(check-expect
 (edit (make-editor "hello world" 4) "g")
 (make-editor "hellgo world" 5))

(check-expect
 (edit (make-editor "hello world" 7) "left")
 (make-editor "hello world" 6))

(check-expect
 (edit (make-editor "hello world" 7) "right")
 (make-editor "hello world" 8))

(define (edit ed ke)
  (cond
    [(= (string-length ke) 1)
     (if (string=? ke "\b")
         (make-editor
          (string-append
           (string-remove-rest
            (editor-text ed)
            (- (editor-index ed) 1))
           (string-rest
            (editor-text ed)
            (editor-index ed)
           ))
          (- (editor-index ed) 1))
         (if
          (and
           (not (string=? ke "\t"))
           (not (string=? ke "\r"))
           (<
            (string-length
             (string-put
              (editor-text ed)
              ke
              (editor-index ed)))
            TEXT-LIMIT))
             (make-editor
              (string-put
               (editor-text ed)
               ke
               (editor-index ed))
               (+ (editor-index ed) 1))
             ed))]
    [(string=? ke "left")
     (make-editor
      (editor-text ed)
      (- (editor-index ed) 1))]
    [(string=? ke "right")
     (make-editor
      (editor-text ed)
      (+ (editor-index ed) 1))]
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

;; tests
(check-expect
 (string-put "hello world" "!" 5)
 "hello! world")

(check-expect
 (string-put "hello world" "!" 11)
 "hello world!")

(define (string-put s c i)
  (string-append
   (string-remove-rest s i)
   c
   (substring s i)))

;; tests
(check-expect
 (string-remove-rest "hello world" 5)
 "hello")

(check-expect
 (string-remove-rest "hello world" 7)
 "hello w")

(define (string-remove-rest s i)
  (substring s 0 i))
