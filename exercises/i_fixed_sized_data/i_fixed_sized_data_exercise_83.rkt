; Exercise 83.

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

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
