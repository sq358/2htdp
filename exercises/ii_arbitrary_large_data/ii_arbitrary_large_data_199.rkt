;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_199) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 199.

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
(define ltracks-example-1 '())
(define ltracks-example-2
  (list track-example1 track-example2))

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))