;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname i_fixed_sized_data_exercise_104) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exerc(ise 104.

(define-struct vehicle
  [passengers plateno fuel-consumption])
; A Vehicle is a structure:
;  (make-vehicle Number String Number)
; interpretation (make-vehicle p s f) is a vehicle with
; p number of passengers it can carry and plate number s
; with f miles/gallon fuel consumption

(define (park-vehicle v)
  ... (vehicle-passengers v) ...
  ... (vehicle-plateno v) ...
  ... (vehicle-fuel-consumption v) ..)