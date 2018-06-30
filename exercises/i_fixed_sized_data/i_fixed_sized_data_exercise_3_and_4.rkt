; Excercise 3. 4.

(define str "helloworld")
(define i 10)

 (cond
   [(and (>= i 0) (< i (string-length str)))
         (string-append
         (substring str 0 (- i 1)) (substring str i))]
   [else "hello"])
