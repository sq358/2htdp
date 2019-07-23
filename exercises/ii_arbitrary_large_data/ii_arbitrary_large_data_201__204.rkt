;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_201__204) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 200, 201, 202, 203, 204

(require 2htdp/batch-io)
(require 2htdp/itunes)

; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last playe

; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

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

(define track-example3
  (create-track
   "sample-track3" "sample-artist1" "sample-album2"
   3333 12 date-example1 2 date-example4))

;; ltracks examples
(define ltracks-example1 '())
(define ltracks-example2
  (list track-example1 track-example2))

(define ltracks-example3
  (list track-example1 track-example2 track-example3))

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

; LTracks -> List-of-Strings
; for given ltracks produces the list of albums

(check-expect (select-album-titles/unique '()) '())
(check-expect
 (select-album-titles/unique ltracks-example2)
 (list "sample-album1" "sample-album2"))
(check-expect
 (select-album-titles/unique ltracks-example3)
 (list "sample-album1" "sample-album2"))

(define (select-album-titles/unique t)
  (cond
    [(empty? t) '()]
    [else
     (create-set
      (cons (track-album (first t))
           (select-album-titles/unique (rest t))))]))

(check-expect (create-set '()) '())
(check-expect (create-set (list "a" "b"))
 (list "a" "b"))
(check-expect
 (create-set (list "a" "b" "a" "c" "b" "d"))
 (list "a" "c" "b" "d"))

(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (if (member (first los) (rest los))
         (create-set (rest los))
         (cons
          (first los)
          (create-set (rest los))))]))

; String LTracks -> LTracks
; for the given album name and ltracks, it produces the
; list of tracks

(check-expect (select-album "test" '()) '())
(check-expect (select-album "test" ltracks-example2) '())
(check-expect
 (select-album "sample-album1" ltracks-example2)
 (list track-example1))

(define (select-album a t)
  (cond
    [(empty? t) '()]
    [else
     (if (string=? (track-album (first t)) a)
         (cons (first t) (select-album a (rest t)))
     (select-album a (rest t)))]))

; Date String LTracks -> LTracks
; for the given date, album and list of tracks, produces the
; list of tracks that belong to the given album and have been
; played after the given date

(check-expect
 (select-album-date date-example2 "sample-album1" '()) '())
(check-expect
 (select-album-date date-example2 "sample-album2" ltracks-example3)
 (list track-example2 track-example3))

(define (select-album-date d a t)
  (cond
    [(empty? t) '()]
    [else
     (if (played-after? d (track-played (first t)))
         (cons (first t) (select-album-date d a (rest t)))
         (select-album-date d a (rest t)))]))

; Date Date -> Boolean
; for given dates determines whether the first occurs after
; the second

(check-expect (played-after? date-example1 date-example1) #false)
(check-expect (played-after? date-example1 date-example2) #false)
(check-expect (played-after? date-example2 date-example1) #true)

(define (played-after? d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #true]
    [(> (date-month d1) (date-month d2)) #true]
    [(> (date-day d1) (date-day d2)) #true]
    [(> (date-hour d1) (date-hour d2)) #true]
    [(> (date-minute d1) (date-minute d2)) #true]
    [(> (date-second d1) (date-second d2)) #true]
    [else #false]))

(define-struct album-ltracks [album ltracks])
; AlbumLTracks is a structure:
;  (make-album-ltracks String LTracks)
; interpretation (make-album-ltracks s lt) is an album-ltracks
; that lists all the tracks for album s

; ListOfAlbumLTracks is one of;
; - '()
; - (cons AlbumLTrack ListOfAlbumLTracks)

; LTracks -> ListOfAlbumLTracks
; for the given ltracks it produces a list of ltracks one per
; album

(check-expect (select-albums '()) '())
(check-expect
 (select-albums ltracks-example3)
 (list
  (make-album-ltracks "sample-album1" (list track-example1))
  (make-album-ltracks "sample-album2" (list track-example2 track-example3))))

(define (select-albums lt)
  (cond
    [(empty? lt) '()]
    [else
     (create-album-tracks
      (select-album-titles/unique lt) lt)]))

; ListOfStrings LTracks -> ListOfAlbumTracks
; for the given list of album titles and list of tracks produces
; the list of album-ltracks

(check-expect (create-album-tracks '() ltracks-example2) '())
(check-expect
 (create-album-tracks
  (list "sample-album1" "sample-album2") ltracks-example3)
 (list
  (make-album-ltracks
   "sample-album1" (list track-example1))
  (make-album-ltracks
   "sample-album2" (list track-example2 track-example3))))

(define (create-album-tracks albums lt)
  (cond
    [(empty? albums) '()]
    [else
     (cons
      (make-album-ltracks
       (first albums) (select-album (first albums) lt))
      (create-album-tracks (rest albums) lt))]))