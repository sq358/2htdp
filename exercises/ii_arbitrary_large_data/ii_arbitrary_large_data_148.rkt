;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ii_arbitrary_large_data_148) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 148.

;; Q. Is it better to work with data definitions that accommodate
;; empty lists as opposed to definitions for non-empty lists?
;; Why? Why not?
;;
;; A. It is better to deal with functions that accomodate empty
;; lists because they're more resilient