; Exercise 61.
;; question: In Figure 27, which of the two functions
;;  are is properly designed using the recipe for
;;  itemization? Which of the two continues to work
;;   if you change the constants to the following
;;    (define RED "red")
;;    (define GREEN "green")
;;    (define YELLOW "yellow")
;;
;; answer: tl-next-symbolic is designed more properly
;;  because it has a cond expression for each of the
;;  sub-classes of data definition, the same function
;;  works with the string constants, whereas
;;  tl-next-numeric doesn't.
