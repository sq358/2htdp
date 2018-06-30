; Exercise 26.
(require 2htdp/image)

(define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))

; Using Stepper, output is 'hellow_orld'
(string-insert "helloworld" 6)

(image-classify <some-image>)
