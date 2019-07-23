;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_220__222) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 220, 221, 222, 223

(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 20) ; # of blocks, vertically
(define SIZE 10) ; blocks are squares
(define WIDTH-SIZE (* WIDTH SIZE))
(define HEIGHT-SIZE (* HEIGHT SIZE))
(define MTCN (empty-scene WIDTH-SIZE HEIGHT-SIZE))

(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define block-dropping (make-block 0 5))
(define block-droped (make-block 0 (- HEIGHT 2)))
(define landscape0 (list block-landed))
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris1 (make-tetris block-droped landscape0))
(define tetris0-drop
  (make-tetris
   (make-block 0 (- HEIGHT 3)) (list block-on-block block-landed)))
(define tetris2
  (make-tetris
   (make-block 2 (- HEIGHT 3))
   (list
    (make-block 3 (- HEIGHT 3))
    (make-block 3 (- HEIGHT 2))
    (make-block 3 (- HEIGHT 1)))))

; main

(define (tetris-main cr)
  (big-bang tetris0
    [to-draw render]
    [on-tick toc cr]
    [on-key move]
    [stop-when game-over?]))

; Tetris -> Image
; render the tetris

(check-expect
 (render tetris0)
 (place-image
  BLOCK
  (+ (* SIZE (block-x (tetris-block tetris0))) (/ SIZE 2))
  (* SIZE (block-y (tetris-block tetris0)))
  (render-landscape (tetris-landscape tetris0))))

(define (render t)
  (place-image
   BLOCK
   (+ (* SIZE (block-x (tetris-block t))) (/ SIZE 2))
   (* SIZE (block-y (tetris-block t)))
   (render-landscape (tetris-landscape t))))

; LandScape -> Image

(check-expect
 (render-landscape landscape0)
 (place-image
  BLOCK
  (+ (* SIZE (block-x (first landscape0))) (/ SIZE 2))
  (* SIZE (block-y (first landscape0)))
  MTCN))

(define (render-landscape l)
  (cond
    [(empty? l) MTCN]
    [else
     (place-image
      BLOCK
      (+ (* SIZE (block-x (first l))) (/ SIZE 2))
      (* SIZE (block-y (first l)))
      (render-landscape (rest l)))]))

; Tetris -> Tetris
; produces the next state of the world

(check-expect
 (toc tetris0)
 (make-tetris
  (make-block
   (block-x (tetris-block tetris0))
   (+ 1 (block-y (tetris-block tetris0))))
  (tetris-landscape tetris0)))

(check-random
 (toc tetris0-drop)
 (make-tetris
  (generate-block (tetris-block tetris0-drop))
  (cons
   (tetris-block tetris0-drop)
   (tetris-landscape tetris0-drop))))
  
(define (toc t)
  (cond
    [(member
      (make-block
       (block-x (tetris-block t))
       (+ (block-y (tetris-block t)) 1)) (tetris-landscape t))
     (make-tetris
      (generate-block (tetris-block t))
      (cons
       (tetris-block t) (tetris-landscape t)))]
    [(> (+ (block-y (tetris-block t)) 1) (- HEIGHT 1))
     (make-tetris
      (generate-block (tetris-block t))
      (cons
       (make-block
        (block-x (tetris-block t)) (- HEIGHT 1))
       (tetris-landscape t)))]
    [else
     (make-tetris
      (make-block
       (block-x (tetris-block t))
       (+ (block-y (tetris-block t)) 1))
       (tetris-landscape t))]))

; Block -> Block
; creates a random dropping block

(check-satisfied
 (generate-block (make-block 0 1)) not-1-1)

(define (generate-block b)
  (check-generate-block
   b (make-block (random WIDTH) 1)))

; Block -> Block
; ensures a duplicate random block is not being created

(check-expect
 (check-generate-block (make-block 3 1) (make-block 5 1))
 (make-block 5 1))

(check-random
 (check-generate-block (make-block 3 1) (make-block 3 10))
 (generate-block (make-block 3 1)))

(define (check-generate-block b1 b2)
  (if (= (block-x b1) (block-x b2))
      (generate-block b1) b2))

; Block -> Boolean
(define (not-1-1 b)
  (not (= (block-x b) 0)))

; Tetris Key -> Tetris
; produces a new tetris based on the input key ke

(check-expect (move tetris0 "down") tetris0)
(check-expect (move tetris0 "up") tetris0)
(check-expect (move tetris0 "h") tetris0)
(check-expect (move tetris0 "left") tetris0)
(check-expect
 (move tetris0 "right")
 (make-tetris (make-block 1 5) (tetris-landscape tetris0)))

(define (move t ke)
  (if (within-bounds? t ke)
      (make-tetris
       (move-block t ke)
       (tetris-landscape t))
      t))

; Tetris Key -> Boolean
; determines if the tetris will be in the scene after the
; move by ke

(check-expect (within-bounds? tetris0 "top") #true)
(check-expect (within-bounds? tetris0 "down") #true)
(check-expect (within-bounds? tetris0 "left") #false)
(check-expect (within-bounds? tetris0 "right") #true)

(define (within-bounds? t ke)
  (cond
    [(string=? ke "left")
     (if (< (sub1 (block-x (tetris-block t))) 0) #false #true)]
    [(string=? ke "right")
     (if
      (>= (add1 (block-x (tetris-block t))) WIDTH) #false #true)]
    [else #true]))

; Tetris Key -> Boolean
; produces a block based on the input key ke

(check-expect
 (move-block tetris2 "right") (make-block 2 (- HEIGHT 3)))
(check-expect
 (move-block tetris0 "right") (make-block 1 5))

(define (move-block t ke)
  (get-block
   (make-block
    (cond
      [(string=? ke "left")
       (sub1 (block-x (tetris-block t)))]
      [(string=? ke "right")
       (add1 (block-x (tetris-block t)))]
      [else (block-x (tetris-block t))])
    (block-y (tetris-block t)))
   t))

(define (get-block b t)
  (if (member b (tetris-landscape t)) (tetris-block t) b))

; Tetris -> Boolean
; determines if the game is over or not

(define (game-over? t)
  (if
   (<= (block-y (first (tetris-landscape t))) 1)
   #true #false))