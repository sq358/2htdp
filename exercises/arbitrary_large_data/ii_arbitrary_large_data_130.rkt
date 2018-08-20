;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_130) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 130.

(cons "Arash"
      (cons "Frank"
            (cons "Simin"
                  (cons "Sara"
                        (cons "Mohsen" '())
                        )
                  )
            )
      )

(cons "1" (cons "2" '()))
; above expression is a List-of-names because it's
; in the form of:
; (cons String List-of-name)

(cons 2 '())
; above expression is not a List-of-name because
; it is not in the form of:
; (cons String List-of-name)

