;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ii_abstraction_258__261) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 258.

; Image Polygon -> Image 
; adds an image of p to MT

(define (render-polygon img p0)
  (local ((define height 20) ; the height of the editor 
          (define width 200) ; its width 
          (define mt (empty-scene width height))
          ; NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots p)
            (cond
              [(empty? (rest p)) mt]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))
          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))])))
    (render-line (connect-dots img p0) (first p0) (last p0))))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; Exercise 259.

(require 2htdp/batch-io)

; On OS X: 
(define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
 
; A Dictionary is a List-of-strings.
(define DICT-AS-LIST (read-lines LOCATION))

; Number Word -> List-of-words
; produces all the arrangement of a word starting at index number
; n

(check-expect (arrange-letters 0 '()) '())
(check-expect (arrange-letters 0 (list "a")) (list (list "a")))
(check-expect (arrange-letters 1 (list "a")) (list (list "a")))
(check-expect
 (arrange-letters 0 (list "a" "b"))
 (list
  (list "b" "a")
  (list "a" "b")))
(check-expect
 (arrange-letters 0 (list "a" "b"))
 (list
  (list "b" "a")
  (list "a" "b")))
(check-expect
 (arrange-letters 0 (list "c" "a" "t"))
 (list
  (list "a" "t" "c")
  (list "a" "c" "t")
  (list "t" "c" "a")
  (list "c" "t" "a")
  (list "c" "a" "t")))

(define (arrange-letters n word)
  (cond
    [(empty? word) '()]
    [(> n (length word)) '()]
    [else
     (local (; Number Word -> 1String
             ; Extracts the letter at index location n from word w
             (define (remove-duplicates low)
               (cond
                 [(empty? low) '()]
                 [else
                  (if (member (first low) (rest low))
                      (remove-duplicates (rest low))
                      (cons (first low) (remove-duplicates (rest low))))]))
             ; Number Word -> 1String
             ; Extracts the letter at index location n from word w
             (define (extract-letter n w)
               (cond
                 [(empty? w) ""]
                 [else
                  (local ((define (extract-letter-at n1 n2 w)
                            (cond
                              [(empty? w) ""]
                              [(> n1 n2) '()]
                              [else
                               (if (= n1 n2)
                                   (first w)
                                   (extract-letter-at (add1 n1) n2 (rest w)))])))
                    (extract-letter-at 0 n w))]))
             ; Number Word -> Word
             ; Removes the letter at index location n from word w
             (define (remove-letter n w)
               (cond
                 [(empty? w) '()]
                 [else
                  (local ((define (remove-letter-at n1 n2 w)
                            (cond
                              [(empty? w) '()]
                              [(> n1 n2) '()]
                              [else
                               (if (= n1 n2)
                                   (rest w)
                                   (cons (first w)
                                         (remove-letter-at (add1 n1) n2 (rest w))))])))
                    (remove-letter-at 0 n w))]))
             ; 1String Word -> List-of-words
             ; inserts the given 1string s to the beginning, in the middle
             ; and at the end of the given word w
             (define (insert-everywhere/in-one-word s w)
               (cond
                 [(empty? w) '()]
                 [else
                  (local
                    ((define (insert-letter n s w)
                       (cond
                         [(empty? w) '()]
                         [(> n (length w)) '()]
                         [else
                          (local (; Number Number 1String Word -> Word
                                  ; inserts the 1string s starting at number n1 and ending at n2
                                  ; everywhere in the word w
                                  (define (insert-letter-at n1 n2 s w)
                                    (cond
                                      [(empty? w) '()]
                                      [(string=? "" s) w]
                                      [else
                                       (if (= n1 n2)
                                           (cons s (cons (first w) (rest w)))
                                           (cons (first w)
                                                 (if (empty? (rest w))
                                                     (cons s '())
                                                     (insert-letter-at (add1 n1) n2 s (rest w)))))])))
                            (cons
                             (insert-letter-at 0 n s w)
                             (insert-letter (add1 n) s w)))])))
                    (insert-letter 0 s w))])))
       (remove-duplicates
        (append
         (insert-everywhere/in-one-word
          (extract-letter n word) (remove-letter n word))
         (arrange-letters (add1 n) word))))]))

; Exercise 260

; :TODO re-evaluate exercise 260 with locals

; Exercise 261.

(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1.v2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define cheap-items (extract1.v2 (rest an-inv))))
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) cheap-items)]
       [else cheap-items]))]))

(extract1.v2 (list
           (make-ir "arash" 10)
           (make-ir "bob" 20)
           (make-ir "Alice" 5)))