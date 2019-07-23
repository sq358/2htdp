;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_275__278) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 275.

(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

(define-struct letter-count [letter count])
; A LetterCount is a structure:
;  (make-letter-count Letter Number)
; Interpretation:
;  (make-letter-count "a" 2) is letter count for letter "a"
;   with count 2

(check-expect (most-frequent '("apple" "bob" "book" "pair"))
              (make-letter-count "b" 2))

(define (most-frequent los)
  (foldl comp-letter-counts
         (make-letter-count "NaN" 0)
         (foldl
          (lambda (w lolc)
            (cons
             (make-letter-count
              (substring w 0 1)
              (add1
               (length
                (filter (lambda (lc)
                          (string=? (letter-count-letter lc)
                                    (substring w 0 1)))
                        lolc))))
            lolc))
         '() los)))

; LetterCount LetterCount -> LetterCount
; from the given letter counts returns the letter count
; with greater count

(define letter-counts
  (list 
   (make-letter-count "a" 10)
   (make-letter-count "b" 2)
   (make-letter-count "c" 11)
   (make-letter-count "d" 1)))

(check-error (comp-letter-counts 2 (make-letter-count "a" 10)))

(check-expect (comp-letter-counts (make-letter-count "a" 10)
                                  (make-letter-count "b" 2))
              (make-letter-count "a" 10))

(define (comp-letter-counts lc1 lc2)
  (cond
    [(or (not (letter-count? lc1))
         (not (letter-count? lc2)))
     (error "Error: arguments must be letter-count")]
    [else
     (if (>= (letter-count-count lc1)
             (letter-count-count lc2))
         lc1 lc2)]))

; Dictionary -> [List-of Dictionary]
; For the given list of strings as dictionary
; produces list of dictionaries for each letter

(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter
               '("apple" "area" "book"))
               '(("book") ("area" "apple")))

(define (words-by-first-letter d)
  (create-dictionaries LETTERS d))

(define (create-dictionaries letters d)
  (filter (lambda (llw)
            (not (empty? llw)))
          (foldl (lambda (l dict)
                   (cons
                    (foldl (lambda (w words)
                             (if (string=? (first (explode w)) l)
                                 (cons w words)
                                 words))
                           '()
                           d)
                    dict)) '() letters)))
 
; Exercise 276.

(require 2htdp/itunes)

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

;; date examples
(define date-example11 (create-date 2018 01 04 10 43 21))
(define date-example12 (create-date 2018 10 09 10 43 21))

(define date-example21 (create-date 2018 02 14 14 03 01))
(define date-example22 (create-date 2018 10 09 11 00 55))

(define date-example31 (create-date 2018 03 15 57 02 10))
(define date-example32 (create-date 2019 01 02 02 01 21))

;; track examples
(define track-example1
  (create-track
   "sample-track1" "sample-artist1" "sample-album1"
   1111 12 date-example11 34 date-example12))

(define track-example2
  (create-track
   "sample-track2" "sample-artist2" "sample-album2"
   2222 12 date-example21 35 date-example22))

(define track-example3
  (create-track
   "sample-track3" "sample-artist3" "sample-album2"
   2222 12 date-example31 35 date-example32))

;; ltracks examples
(define ltracks-example1 '())
(define ltracks-example2
  (list track-example1 track-example2 track-example3))

; String Date LTracks
; from the given ltracks, extracts the list of tracks from the
; given album that have been played after the date

(check-expect (select-album-date "sample-album0"
                                 date-example21
                                 ltracks-example2)
              '())

(check-expect (select-album-date "sample-album2"
                                 (create-date 2017 12 04 0 0 0)
                                 ltracks-example2)
              (list track-example2 track-example3))

(define (select-album-date a d lt)
  (filter (lambda (t) (played-after? (track-played t) d))
          (filter (lambda (t) (string=? (track-album t) a)) lt)))

; Date Date -> Boolean
; for given dates determines whether the first occurs after
; the second

(check-expect (played-after? date-example11 date-example11) #false)
(check-expect (played-after? date-example11 date-example12) #false)
(check-expect (played-after? date-example12 date-example11) #true)

(define (played-after? d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #true]
    [(> (date-month d1) (date-month d2)) #true]
    [(> (date-day d1) (date-day d2)) #true]
    [(> (date-hour d1) (date-hour d2)) #true]
    [(> (date-minute d1) (date-minute d2)) #true]
    [(> (date-second d1) (date-second d2)) #true]
    [else #false]))

; Exercise 277.

(require 2htdp/universe)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; List-of-missiles is one of:
; - '()
; - (cons Missle List-of-missiles)

(define-struct sigs.v4 [ufo tank missiles])
; A SIGS.v4 is a structure:
;   (make-sigs.v4 UFO Tank List-of-missiles)
; interpretation represents the complete state of a
; space invader game

; constants
(define SKY-HEIGHT 450)
(define GROUND-HEIGHT 30)

(define HEIGHT
  (+ SKY-HEIGHT GROUND-HEIGHT))

(define WIDTH 500)

(define BACKGROUND
  (above
   (rectangle WIDTH SKY-HEIGHT "solid" "black")
   (rectangle WIDTH GROUND-HEIGHT "solid" "brown")))

(define TANK-SPEED 3)
(define UFO-SPEED 2)
(define MISSILE-SPEED (* UFO-SPEED 3))

(define TANK-WIDTH 35)
(define TANK-HEIGHT 15)
(define MISSILE-SIZE 9)
(define UFO-BASE-WIDTH 30)
(define UFO-BASE-HEIGHT 15)

(define PROXIMITY 15) ; in pixels

(define TANK-COLOR "green")
(define MISSILE-COLOR "red")
(define UFO-BASE-COLOR "orange")
(define UFO-CAP-COLOR "purple")

(define TANK
  (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))

(define MISSILE
  (triangle MISSILE-SIZE "solid" MISSILE-COLOR))

(define UFO-BASE
  (rectangle
   UFO-BASE-WIDTH UFO-BASE-HEIGHT "solid" UFO-BASE-COLOR))
(define UFO-CAP
  (ellipse
   UFO-BASE-HEIGHT UFO-BASE-WIDTH "solid" UFO-CAP-COLOR))
(define UFO
  (overlay UFO-CAP UFO-BASE))

(define GAME-OVER
  (text "Game over!" 11 "red"))

(define PLAYER-WON
  (text "Player won!" 11 "blue"))

(define TEST-TANK-GOES-RIGHT (make-tank 10 3))
(define TEST-TANK-GOES-LEFT (make-tank 10 -3))

(define TEST-UFO (make-posn 50 30))
(define TEST-TANK TEST-TANK-GOES-RIGHT)
(define TEST-MISSILE (make-posn 10 50))

(define TEST-SIGSV4-NO-MISSILE
  (make-sigs.v4 TEST-UFO TEST-TANK '()))

(define TEST-SIGSV4-MISSILE
  (make-sigs.v4 TEST-UFO TEST-TANK (list TEST-MISSILE)))

(define TEST-SIGSV4-GAME-OVER-1
  (make-sigs.v4
   (make-posn 20 HEIGHT) TEST-TANK-GOES-RIGHT '()))

(define TEST-SIGSV4-GAME-OVER-2
  (make-sigs.v4
   (make-posn 70 30)
   (make-tank 70 3)
   (list (make-posn 70 (+ 30 (/ PROXIMITY 2))))))

; Big bang
(define (si-main.v4 s)
  (big-bang s
    [to-draw si-render.v4]
    [on-tick si-move.v4]
    [on-key si-control.v4]
    [stop-when si-game-over.v4? si-render-final.v4]
    [state #true]))

; sigs.v4 -> Image
; renders the given game stage as image
(check-expect
 (si-render.v4 TEST-SIGSV4-NO-MISSILE)
 (tank-render.v4
  (sigs.v4-tank TEST-SIGSV4-NO-MISSILE)
  (ufo-render.v4
   (sigs.v4-ufo TEST-SIGSV4-NO-MISSILE)
   (missiles-render
    (sigs.v4-missiles TEST-SIGSV4-NO-MISSILE)
    BACKGROUND))))

(check-expect
 (si-render.v4 TEST-SIGSV4-MISSILE)
 (tank-render.v4
  (sigs.v4-tank TEST-SIGSV4-MISSILE)
  (ufo-render.v4
   (sigs.v4-ufo TEST-SIGSV4-MISSILE)
   (missiles-render
    (sigs.v4-missiles TEST-SIGSV4-MISSILE)
    BACKGROUND))))

(define (si-render.v4 s)
  (tank-render.v4
   (sigs.v4-tank s)
   (ufo-render.v4
    (sigs.v4-ufo s)
    (missiles-render
     (sigs.v4-missiles s)
     BACKGROUND))))

; Tank Image -> Image
; adds an image of tank t to scene s

;; tests
(check-expect
 (tank-render.v4 TEST-TANK BACKGROUND)
 (place-image
  TANK (tank-loc TEST-TANK) SKY-HEIGHT BACKGROUND))

(define (tank-render.v4 t s)
  (place-image
   TANK (tank-loc t) SKY-HEIGHT s))

; UFO Image -> Image
; adds an image of ufo u to scene s

;; tests
(check-expect
 (ufo-render.v4 TEST-UFO BACKGROUND)
 (place-image
  UFO (posn-x TEST-UFO) (posn-y TEST-UFO) BACKGROUND))

(define (ufo-render.v4 u s)
  (place-image
   UFO (posn-x u) (posn-y u) s))

; List-of-missiles Image -> Image
; renders missiles to the scene s

(check-expect (missiles-render '() BACKGROUND) BACKGROUND)
(check-expect
 (missiles-render
  (list
   (make-posn 10 15)
   (make-posn 10 14)
   (make-posn 10 13))
 BACKGROUND)
 (place-image
  MISSILE
  10
  15
  (place-image
   MISSILE
   10
   14
   (place-image
    MISSILE
    10
    13
    BACKGROUND))))

(define (missiles-render lm im)
  (foldr (lambda (m s)
           (place-image MISSILE (posn-x m) (posn-y m) s))
         im lm))

; SIGS.v4 -> Boolean
; determines if the game is over.
; returns #true if UFO lands with proximy PROXIMITY
; to HEIGHT or one of the missiles hits the UFO with proximity
; PROXIMITY.

(check-expect
 (si-game-over.v4? TEST-SIGSV4-NO-MISSILE) #false)

(check-expect
 (si-game-over.v4? TEST-SIGSV4-MISSILE) #false)

(check-expect
 (si-game-over.v4? TEST-SIGSV4-GAME-OVER-1) #true)

(check-expect
 (si-game-over.v4? TEST-SIGSV4-GAME-OVER-2) #true)

(define (si-game-over.v4? s)
  (cond
    [(empty? (sigs.v4-missiles s))
     (> (posn-y (sigs.v4-ufo s)) (- HEIGHT PROXIMITY))]
    [(cons? (sigs.v4-missiles s))
     (or
      (any-missile-hit-ufo? s)
      (> (posn-y (sigs.v4-ufo s)) (- HEIGHT PROXIMITY)))]
    ))

; SIGS.v4 -> Image
; renders game scene after game over

;; tests
(check-expect
 (si-render-final.v4 TEST-SIGSV4-GAME-OVER-1)
 (place-image
  GAME-OVER (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(check-expect
 (si-render-final.v4 TEST-SIGSV4-GAME-OVER-2)
 (place-image
  PLAYER-WON (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

(define (si-render-final.v4 s)
  (place-image
   (cond
     [(empty? (sigs.v4-missiles s)) GAME-OVER]
     [else
      (if (any-missile-hit-ufo? s) PLAYER-WON GAME-OVER)])
   (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))

; SIGS.v4 -> Boolean
; determines if any of the missiles hit the ufo

(check-expect (any-missile-hit-ufo? TEST-SIGSV4-NO-MISSILE) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV4-MISSILE) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV4-GAME-OVER-1) #false)
(check-expect (any-missile-hit-ufo? TEST-SIGSV4-GAME-OVER-2) #true)

(define (any-missile-hit-ufo? s)
  (foldl (lambda (m accum)
           (or
            (missile-hit-ufo? m (sigs.v4-ufo s))
            accum))
         #false (sigs.v4-missiles s)))

(define (missile-hit-ufo? m u)
  (and
   (<= (abs (- (posn-y m) (posn-y u))) PROXIMITY)
   (<= (abs (- (posn-x m) (posn-x u))) PROXIMITY)))

; SIGS.v4 -> SIGS.v4
; for every clock tick creates the next space-invader

;; tests
(check-random
 (si-move.v4
  (make-sigs.v4 (make-posn 10 40) (make-tank 30 2) '()))
  (si-move-proper.v4
   (make-sigs.v4 (make-posn 10 40) (make-tank 30 2) '())
   (ufo-random-jump (make-posn 10 40)))
  )

(check-random
 (si-move.v4
  (make-sigs.v4
   (make-posn 10 40) (make-tank 30 2) (list (make-posn 20 40))))
  (si-move-proper.v4
   (make-sigs.v4
    (make-posn 10 40) (make-tank 30 2) (list (make-posn 20 40)))
   (ufo-random-jump (make-posn 10 40)))
  )

(define (si-move.v4 s)
  (si-move-proper.v4 s (ufo-random-jump (sigs.v4-ufo s))))

; UFO -> Number
; for input u returns n where n can be negative
; or positive, and (abs n) is within [0, WIDTH) value

(check-random
 (ufo-random-jump (make-posn 5 12))
 (* -1 (random 3)))

(check-random
 (ufo-random-jump (make-posn 12 20))
 (random 3))

(define (ufo-random-jump u)
 (cond
   [(= (modulo (posn-x u) 2) 0) (random 3)]
   [else (* -1 (random 3))]))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta

;; tests
(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 40 3) '()) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) '()))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 40 -3) '()) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) '()))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 10 3) '()) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) '()))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 10 -3) '()) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) '()))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 40 3) (list (make-posn 35 60))) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 43 3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 40 -3) (list (make-posn 35 60))) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 37 -3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 10 3) (list (make-posn 35 60))) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 7 3) (list (make-posn 35 54))
  ))

(check-expect
 (si-move-proper.v4
  (make-sigs.v4
   (make-posn 10 12) (make-tank 10 -3) (list (make-posn 35 60))) 2)
 (make-sigs.v4
  (make-posn 12 (+ 12 UFO-SPEED)) (make-tank 13 -3) (list (make-posn 35 54))
  ))

(define (si-move-proper.v4 s delta)
  (make-sigs.v4
   (make-posn
    (+ (posn-x (sigs.v4-ufo s)) delta)
    (+ (posn-y (sigs.v4-ufo s)) UFO-SPEED))
   (if (tank-is-out? (sigs.v4-tank s))
       (make-tank
        (- (tank-loc (sigs.v4-tank s)) (tank-vel (sigs.v4-tank s)))
        (tank-vel (sigs.v4-tank s)))
       (move-tank (sigs.v4-tank s)))
   (move-missiles (sigs.v4-missiles s))))

; List-of-missiles -> List-of-missiles
; for every clock tick, moves missiles with missiles speed

(check-expect (move-missiles '()) '())
(check-expect
 (move-missiles (list (make-posn 35 60) (make-posn 35 70)))
 (list (make-posn 35 54) (make-posn 35 64)))

(define (move-missiles lm)
  (map (lambda (m)
         (make-posn (posn-x m)
                    (- (posn-y m) MISSILE-SPEED)))
       lm))

(define (tank-is-out? t)
  (or
   (>=
    (tank-loc (move-tank t)) (- WIDTH (/ (image-width TANK) 2)))
   (<=
    (tank-loc (move-tank t)) (/ (image-width TANK) 2))))

; Tank -> Tank
; moves tank t based on tank-vel
;; tests
(check-expect
 (move-tank (make-tank 10 -3)) (make-tank 7 -3))
(check-expect
 (move-tank (make-tank 10 3)) (make-tank 13 3))

(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t)))

; SIGS.v4 KeyEvent -> SIGS.v4
; based on key event ke producses a new space-invader state
; pressing the left arrow ensures that the tank moves left;
; pressing the right arrow ensures that the tank moves right; and
; pressing the space bar fires the missile if it hasn’t been launched yet.

(check-expect
 (si-control.v4
  (make-sigs.v4
    (make-posn 10 12) (make-tank 10 3) '()) "left")
 (make-sigs.v4
  (make-posn 10 12) (make-tank 10 -3) '()))

(check-expect
 (si-control.v4
  (make-sigs.v4
    (make-posn 10 12) (make-tank 10 -3) '()) "right")
 (make-sigs.v4
  (make-posn 10 12) (make-tank 10 3) '()))

(check-expect
 (si-control.v4
  (make-sigs.v4
    (make-posn 10 12) (make-tank 10 -3) '()) " ")
 (make-sigs.v4
  (make-posn 10 12) (make-tank 10 -3)
  (list (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT)))
  ))

(check-expect
 (si-control.v4
  (make-sigs.v4
    (make-posn 10 12)
    (make-tank 10 -3)
    (list (make-posn 10 (- (- SKY-HEIGHT TANK-HEIGHT) 2)))) " ")
 (make-sigs.v4
  (make-posn 10 12)
  (make-tank 10 -3)
  (list
   (make-posn 10 (- SKY-HEIGHT TANK-HEIGHT))
   (make-posn 10 (- (- SKY-HEIGHT TANK-HEIGHT) 2)))))

(check-expect
 (si-control.v4
  (make-sigs.v4
    (make-posn 10 12) (make-tank 10 -3) '()) "u")
 (make-sigs.v4
  (make-posn 10 12) (make-tank 10 -3) '()))

(define (si-control.v4 s k)
  (cond
    [(or (string=? k "left") (string=? k "right"))
     (make-sigs.v4
      (sigs.v4-ufo s)
      (next-tank (sigs.v4-tank s) k)
      (sigs.v4-missiles s))]
    [(string=? k " ")
     (make-sigs.v4
      (sigs.v4-ufo s)
      (sigs.v4-tank s)
      (cons
       (make-posn
        (tank-loc (sigs.v4-tank s)) (- SKY-HEIGHT TANK-HEIGHT))
       (sigs.v4-missiles s)))]
    [else s]))

; Number KeyEvent -> Number
; returns next tank based on KeyEvent ke

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "left") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "right") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "left") TEST-TANK-GOES-LEFT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "right") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-RIGHT "h") TEST-TANK-GOES-RIGHT)

(check-expect
 (next-tank TEST-TANK-GOES-LEFT "y") TEST-TANK-GOES-LEFT)

(define (next-tank t k)
  (cond
    [(string=? k "left")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (tank-vel t) (* -1 (tank-vel t))))]
    [(string=? k "right")
     (make-tank
      (tank-loc t)
      (if (< (tank-vel t) 0) (* -1 (tank-vel t)) (tank-vel t)))]
    [else t]))

(si-main.v4
  (make-sigs.v4   (make-posn (/ WIDTH 2) 10) (make-tank (/ WIDTH 2) 3) '()))

; Exercise 278