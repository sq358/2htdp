;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_329) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 329.

;; Q. How many times does a file name read! occur in the directory tree TS?
;; A. 2 times

;; Q. Can you describe the path from the root directory to the occurrences?
;; A. TS --> read!, TS --> Libs --> Docs --> read!

;; Q. What is the total size of all the files in the tree?
;; A. 207

;; Q. What is the total size of the directory if each directory node has size 1?
;; 5

;; Q. How many levels of directories does it contain?
;; A. 2 levels