;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_129) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 129.

(cons "Jupiter"
      (cons "Saturn"
            (cons "Venus"
                  (cons "Mars"
                        (cons "Earth"
                              (cons "Neptune"
                                    (cons "Mercury"
                                          (cons "Uranus" '())
                                          )
                                    )
                              )
                        )
                  )
            )
      )

(cons "Steak"
      (cons "French fries"
            (cons "beans"
                  (cons "bread" '())
                  )
            )
      )

(cons "Yellow"
      (cons "Blue"
            (cons "Red" '())))
