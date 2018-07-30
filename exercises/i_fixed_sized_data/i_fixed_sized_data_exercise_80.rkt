; Exercise 80.

(define-struct movie [title director year])

(define (classic-movie m)
  (...
   (move-year m) ...
   (movie-director m ) ...
   (movie-title m)))

(define-struct pet [name number])
(define (feed-pet p)
  (... (pet-name p) ... (pet-number p)))

(define-struct CD [artist title price])
(define (register-record c)
  (...
   (CD-artist c) ...
   (CD-title c) ...
   (CD-price c)))

(define-struct sweater [material size color])
(define (match-sweater s)
  (...
   (sweater-material s) ...
   (sweater-size s) ...
   (sweater-color s) ...))