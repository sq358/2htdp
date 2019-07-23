;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ii_arbitrary_large_data_209__211) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 209, 210, 211, 212, 214

(require 2htdp/batch-io)

; On OS X: 
(define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
 
; A Dictionary is a List-of-strings.
(define DICT-AS-LIST (read-lines LOCATION))

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
 
; A List-of-words is one of:
; - '() or
; - (cons Word ListOfWords

; List-of-words -> List-of-strings
; turns all Words in low into Strings

(check-expect (words->strings '()) '())
(check-expect
 (words->strings
  (list (string->word "hello") (string->word "world")))
 (list
  (word->string (list "h" "e" "l" "l" "o"))
  (word->string (list "w" "o" "r" "l" "d"))))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else
     (cons
      (word->string (first low))
      (words->strings (rest low)))]))

; String -> Word
; converts s to the chosen word representation

(check-expect (string->word "") '())
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))

(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string

(check-expect (word->string '()) "")
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")

(define (word->string w)
  (implode w))

; String -> List-of-words
; produces list of alternative words

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))

(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))

; Word -> List-of-words
; finds all rearrangements of word

(check-expect (arrangements '()) '())
(check-expect
 (arrangements (list "c" "a" "t"))
 (list
  (list "a" "t" "c")
  (list "a" "c" "t")
  (list "t" "c" "a")
  (list "c" "t" "a")
  (list "c" "a" "t")))

(define (arrangements word)
  (cond
    [(empty? word) '()]
    [else
     (arrange-letters 0 word)]))

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
     (remove-duplicates
      (append
       (insert-everywhere/in-one-word
        (extract-letter n word) (remove-letter n word))
       (arrange-letters (add1 n) word)))]))

(define (remove-duplicates low)
  (cond
    [(empty? low) '()]
    [else
     (if (member (first low) (rest low))
          (remove-duplicates (rest low))
          (cons (first low) (remove-duplicates (rest low))))]))

; Number Word -> 1String
; Extracts the letter at index location n from word w

(check-expect (extract-letter 0 '()) "")
(check-expect (extract-letter 0 (list "e")) "e")
(check-expect (extract-letter 0 (list "e" "f")) "e")
(check-expect (extract-letter 1 (list "e" "f")) "f")
(check-expect (extract-letter 0 (list "e" "f")) "e")
(check-expect (extract-letter 0 (list "e" "f" "g")) "e")
(check-expect (extract-letter 1 (list "e" "f" "g")) "f")
(check-expect (extract-letter 2 (list "e" "f" "g")) "g")

(define (extract-letter n w)
  (cond
    [(empty? w) ""]
    [else
     (extract-letter-at 0 n w)]))

(define (extract-letter-at n1 n2 w)
  (cond
    [(empty? w) ""]
    [(> n1 n2) '()]
    [else
     (if (= n1 n2)
         (first w)
         (extract-letter-at (add1 n1) n2 (rest w)))]))

; Number Word -> Word
; Removes the letter at index location n from word w

(check-expect (remove-letter 0 '()) '())
(check-expect (remove-letter 0 (list "e")) '())
(check-expect (remove-letter 0 (list "e" "f")) (list "f"))
(check-expect (remove-letter 1 (list "e" "f")) (list "e"))
(check-expect
 (remove-letter 0 (list "e" "f" "g"))
 (list "f" "g"))
(check-expect
 (remove-letter 1 (list "e" "f" "g"))
 (list "e" "g"))
(check-expect
 (remove-letter 2 (list "e" "f" "g"))
 (list "e" "f"))

(define (remove-letter n w)
  (cond
    [(empty? w) '()]
    [else
     (remove-letter-at 0 n w)]))

(define (remove-letter-at n1 n2 w)
  (cond
    [(empty? w) '()]
    [(> n1 n2) '()]
    [else
     (if (= n1 n2)
        (rest w)
        (cons (first w)
              (remove-letter-at (add1 n1) n2 (rest w))))]))

; 1String List-of-words -> List-of-words
; inserts 1string s at the beginning, between all letters
; and at the end of all words in list of words

(check-expect (insert-everywhere/in-all-words "e" '()) '())
(check-expect
 (insert-everywhere/in-all-words "e" (list (list "a")))
 (list (list "e" "a") (list "a" "e")))

(check-expect
 (insert-everywhere/in-all-words
  "e" (list (list "a" "b")))
 (list
  (list "e" "a" "b") (list "a" "e" "b") (list "a" "b" "e")))

(check-expect
 (insert-everywhere/in-all-words
  "e" (list (list "a" "b" "c")))
 (list
  (list "e" "a" "b" "c")
  (list "a" "e" "b" "c")
  (list "a" "b" "e" "c")
  (list "a" "b" "c" "e")))

(check-expect
 (insert-everywhere/in-all-words
  "e" (list (list "a" "b" "c" "d")))
 (list
  (list "e" "a" "b" "c" "d")
  (list "a" "e" "b" "c" "d")
  (list "a" "b" "e" "c" "d")
  (list "a" "b" "c" "e" "d")
  (list "a" "b" "c" "d" "e")))

(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) '()]
    [else
     (append (insert-everywhere/in-one-word s (first low))
             (insert-everywhere/in-all-words s (rest low)))]))
       
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
; (define (in-dictionary los) '())

(check-expect (in-dictionary '()) '())
(check-expect
 (in-dictionary (list "dear" "drae" "boat" "baot"))
 (list "dear" "boat"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else
     (if (member (first los) DICT-AS-LIST)
         (cons (first los) (in-dictionary (rest los)))
         (in-dictionary (rest los)))]))

          
; 1String Word -> List-of-words
; inserts the given 1string s to the beginning, in the middle
; and at the end of the given word w

(check-expect (insert-everywhere/in-one-word "d" '()) '())
(check-expect
 (insert-everywhere/in-one-word "d" (list "a"))
 (list
  (list "d" "a")
  (list "a" "d")))
(check-expect
 (insert-everywhere/in-one-word "d" (list "a" "b"))
 (list
  (list "d" "a" "b")
  (list "a" "d" "b")
  (list "a" "b" "d")))
(check-expect
 (insert-everywhere/in-one-word "c" (list "a" "t"))
 (list
  (list "c" "a" "t")
  (list "a" "c" "t")
  (list "a" "t" "c")))
(check-expect
 (insert-everywhere/in-one-word "d" (list "a" "b" "c"))
 (list
  (list "d" "a" "b" "c")
  (list "a" "d" "b" "c")
  (list "a" "b" "d" "c")
  (list "a" "b" "c" "d")))

(define (insert-everywhere/in-one-word s w)
  (cond
    [(empty? w) '()]
    [else
     (insert-letter 0 s w)]))

; Number 1String Word -> ListOfWords
; inserts 1string s starting at position n to everywhere in the word

(check-expect (insert-letter 0 "e" '()) '())
(check-expect
 (insert-letter 0 "e" (list "a"))
 (list
  (list "e" "a")
  (list "a" "e")))
(check-expect
 (insert-letter 0 "e" (list "a" "b"))
 (list
  (list "e" "a" "b")
  (list "a" "e" "b")
  (list "a" "b" "e")))
(check-expect
 (insert-letter 0 "e" (list "a" "b" "c"))
 (list
  (list "e" "a" "b" "c")
  (list "a" "e" "b" "c")
  (list "a" "b" "e" "c")
  (list "a" "b" "c" "e")))

(define (insert-letter n s w)
  (cond
    [(empty? w) '()]
    [(> n (length w)) '()]
    [else
     (cons
      (insert-letter-at 0 n s w)
      (insert-letter (add1 n) s w))]))

; Number Number 1String Word -> Word
; inserts the 1string s starting at number n1 and ending at n2
; everywhere in the word w

(check-expect (insert-letter-at 0 0 "e" '()) '())
(check-expect
 (insert-letter-at 0 0 "e" (list "a"))
 (list "e" "a"))
(check-expect
 (insert-letter-at 0 1 "e" (list "a"))
 (list "a" "e"))
(check-expect
 (insert-letter-at 0 0 "e" (list "a" "b"))
 (list "e" "a" "b"))
(check-expect
 (insert-letter-at 0 1 "e" (list "a" "b"))
 (list "a" "e" "b"))
(check-expect
 (insert-letter-at 0 2 "e" (list "a" "b"))
 (list "a" "b" "e"))
(check-expect
 (insert-letter-at 0 0 "e" (list "a" "b" "c"))
 (list "e" "a" "b" "c"))
(check-expect
 (insert-letter-at 0 1 "e" (list "a" "b" "c"))
 (list "a" "e" "b" "c"))
(check-expect
 (insert-letter-at 0 2 "e" (list "a" "b" "c"))
 (list "a" "b" "e" "c"))
(check-expect
 (insert-letter-at 0 3 "e" (list "a" "b" "c"))
 (list "a" "b" "c" "e"))

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
                   (insert-letter-at (add1 n1) n2 s (rest w)))))]))