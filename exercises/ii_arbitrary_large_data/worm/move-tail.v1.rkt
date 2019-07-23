;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname move-tail.v1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Tail String -> Tail
; moves tail in the direction s

(check-expect (move-tail.v1 '() "left") '())
(check-expect
 (move-tail.v1
  (list (make-worm-segment 10 40) (make-worm-segment 11 40)) "left")
 (move-worm-tail/direction
  "left"
  (worm-segment-from-top (make-worm-segment 10 40))
  (list (make-worm-segment 10 40) (make-worm-segment 11 40))))

(check-expect
 (move-tail.v1
  (list (make-worm-segment 10 40) (make-worm-segment 11 40)) "up")
 (move-worm-tail/direction
  "up"
  (worm-segment-from-left (make-worm-segment 10 40))
  (list (make-worm-segment 10 40) (make-worm-segment 11 40))))

(define (move-tail.v1 tail d)
  (cond
    [(empty? tail) '()]
    [else
     (move-worm-tail/direction
      d (if (or (string=? d "up") (string=? d "down"))
            (worm-segment-from-left (first tail))
            (worm-segment-from-top (first tail))) tail)]))

; String Number Tail -> Tail

(check-expect
 (move-worm-tail/direction
  "down" 10 (list (make-worm-segment 10 40) (make-worm-segment 11 40)))
 (list (make-worm-segment 10 41) (make-worm-segment 10 40)))

(define (move-worm-tail/direction d n t)
  (cond
    [(empty? t) '()]
    [else
     (if (or (string=? d "left") (string=? d "right"))
         (move-worm-tail/h d n t)
         (move-worm-tail/v d n t))]))

; String Number Tail -> Tail
; moves worm horizontally

(check-expect (move-worm-tail/h "left" 40 '()) '())
(check-expect
 (move-worm-tail/h
  "left" 40 (list (make-worm-segment 10 40) (make-worm-segment 10 39)))
 (list (make-worm-segment 9 40) (make-worm-segment 10 40)))
(check-expect
 (move-worm-tail/h
  "right" 40 (list (make-worm-segment 10 40) (make-worm-segment 10 39)))
 (list (make-worm-segment 11 40) (make-worm-segment 10 40)))
(check-expect
 (move-worm-tail/h
  "up" 10 (list (make-worm-segment 10 40) (make-worm-segment 10 39)))
 (list (make-worm-segment 10 40) (make-worm-segment 10 39)))

(define (move-worm-tail/h d n t)
  (cond
    [(empty? t) '()]
    [(or (string=? d "up") (string=? d "down")) t]
    [(= (worm-segment-from-top (first t)) n)
        (cons
         (make-worm-segment
          (cond
            [(string=? d "left")
             (- (worm-segment-from-left (first t)) 1)]
            [(string=? d "right")
             (+ (worm-segment-from-left (first t)) 1)])
          (worm-segment-from-top (first t)))
      (move-worm-tail/h d n (rest t)))]
    [(< (worm-segment-from-top (first t)) n)
     (cons
      (make-worm-segment
       (worm-segment-from-left (first t))
       (+ (worm-segment-from-top (first t)) 1))
      (move-worm-tail/h d n (rest t)))]
    [(> (worm-segment-from-top (first t)) n)
     (cons
      (make-worm-segment
       (worm-segment-from-left (first t))
       (- (worm-segment-from-top (first t)) 1))
      (move-worm-tail/h d n (rest t)))]))

; String Number Tail -> Tail
; moves worm vertically

(check-expect (move-worm-tail/v "left" 40 '()) '())
(check-expect
 (move-worm-tail/v
  "up" 10 (list (make-worm-segment 10 40) (make-worm-segment 11 40)))
 (list (make-worm-segment 10 39) (make-worm-segment 10 40)))
(check-expect
 (move-worm-tail/v
  "down" 10 (list (make-worm-segment 10 40) (make-worm-segment 9 40)))
 (list (make-worm-segment 10 41) (make-worm-segment 10 40)))
(check-expect
 (move-worm-tail/v
  "left" 40 (list (make-worm-segment 10 40) (make-worm-segment 11 40)))
 (list (make-worm-segment 10 40) (make-worm-segment 11 40)))

(define (move-worm-tail/v d n t)
  (cond
    [(empty? t) '()]
    [(or (string=? d "left") (string=? d "right")) t]
    [(= (worm-segment-from-left (first t)) n)
        (cons
         (make-worm-segment
          (worm-segment-from-left (first t))
          (cond
            [(string=? d "up")
             (- (worm-segment-from-top (first t)) 1)]
            [(string=? d "down")
             (+ (worm-segment-from-top (first t)) 1)]))
      (move-worm-tail/v d n (rest t)))]
    [(< (worm-segment-from-left (first t)) n)
     (cons
      (make-worm-segment
       (+ (worm-segment-from-left (first t)) 1)
       (worm-segment-from-top (first t)))
      (move-worm-tail/v d n (rest t)))]
    [(> (worm-segment-from-left (first t)) n)
     (cons
      (make-worm-segment
       (- (worm-segment-from-left (first t)) 1)
       (worm-segment-from-top (first t)))
      (move-worm-tail/v d n (rest t)))]))