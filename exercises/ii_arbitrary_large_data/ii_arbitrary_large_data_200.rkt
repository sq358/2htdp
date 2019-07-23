;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_200) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 200.

(require 2htdp/batch-io)
(require 2htdp/itunes)

;; date examples
(define date-example1 (create-date 2018 01 04 10 43 21))
(define date-example2 (create-date 2018 10 09 10 43 21))
(define date-example3 (create-date 2018 02 14 14 03 01))
(define date-example4 (create-date 2018 10 09 11 00 55))

;; track examples
(define track-example1
  (create-track
   "sample-track1" "sample-artist1" "sample-album1"
   1111 12 date-example1 34 date-example2))

(define track-example2
  (create-track
   "sample-track2" "sample-artist2" "sample-album2"
   2222 12 date-example3 35 date-example4))

;; ltracks examples
(define ltracks-example1 '())
(define ltracks-example2
  (list track-example1 track-example2))

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; LTracks -> Number
; produces the total amount of play time

(check-expect (total-time '()) 0)
(check-expect (total-time ltracks-example2) (+ 1111 2222))

(define (total-time t)
  (cond
    [(empty? t) 0]
    [else (+ (track-time (first t)) (total-time (rest t)))]))