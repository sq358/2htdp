;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_338__344) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; Exercise 338

(define DEV-NULL
  (make-dir "/dev/null" '() '()))

(define MY-DIR
  "/Users/maymandi/Workdocs/files/dev/bitbucket/racketlang/arash/")

(define MY-DT (create-dir MY-DIR))

(define A-DT
  (make-dir
   "TS"
   `(,(make-dir
       "Text"
       '()
       `(,(make-file "part1" 99 "")
         ,(make-file "part2" 52 "")
         ,(make-file "part3" 17 "")))
     ,(make-dir
       "Libs"
       `(,(make-dir
           "Code"
           '()
           `(,(make-file "hang" 8 "")
             ,(make-file "draw" 2 "")))
         ,(make-dir
           "Docs"
           '()
           `(,(make-file "read!" 19 ""))))
       '()))
   `(,(make-file "read!" 10 ""))))

(check-expect (how-many (make-dir "Books" '() '())) 0)
(check-expect (how-many MY-DT) 307)

(define (how-many dt)
  (foldr (lambda (d n) (+ (how-many d) n))
         (length (dir-files dt))
         (dir-dirs dt)))

; Q. Why are you confident that how-many produces correct
;  results for these directories?
; A.
; $ MY-FILES="/Users/maymandi/Workdocs/files/dev/bitbucket/racketlang/arash/"
; $ find $MY-FILES -type f -print | wc -l
;     307

; Exercise 339.

; Dir File -> Boolean
; Determines if the given file is in the given directory

(check-expect (find? DEV-NULL "test")
              #false)

(check-expect (find? MY-DT "space-invader.rkt")
              #true)

(define (find? dt f)
  (foldr (lambda (d r)
           (or r (find? d f)))
         (member f (foldr (lambda (a-file all-files)
                            (cons (file-name a-file) all-files))
                          '()
                          (dir-files dt)))
         (dir-dirs dt)))

; Exercise 340.

; Dir -> [List-of String]
; Lists the name of files and directories in the given
; directory

(check-expect (ls DEV-NULL) '())
(check-expect (ls A-DT)
              '("Text" "part1" "part2" "part3"
                       "Libs" "Code" "hang" "draw"
                       "Docs" "read!" "read!"))

(define (ls dt)
  (foldr (lambda (d names)
           (cons (dir-name d) (append (ls d) names)))
         (foldr (lambda (f fnames)
                  (cons (file-name f) fnames))
                '()
                (dir-files dt))
         (dir-dirs dt)))

; Exercise 341.

; Dir -> N
; Computes the total size of all the files in the
; entire given directory

(check-expect (du DEV-NULL) 0)
(check-expect (du A-DT) 211)

(define (du dt)
  (foldr (lambda (d n)
           (+ (du d) n))
         (+ (length (dir-dirs dt))
            (foldr (lambda (f sz)
                  (+ (file-size f) sz))
                0
                (dir-files dt)))
         (dir-dirs dt)))

; A Path is [List-of String].
; interpretation directions into a directory tree

; Exercise 342.

; A FindOutut is one of:
; - #false
; - Path

; Dir String -> FindOutput
; If exists, returns the path from the given dir to the file
; otherwise returns #false

(check-expect (find DEV-NULL "foo") #false);
(check-expect (find.v1 A-DT "part1")
              '("TS" "Text" "part1"))

(define (find dt f)
  (find-accum dt f #false
              (list (dir-name dt))))

(define (find-accum dt f found path)
  (cond
    [found path]
    [(member f (dir-filenames dt))
     (find-accum dt f #true (append path (list f)))]
    [else
     (find-in-dirs (dir-dirs dt) f found path)]))

(define (find-in-dirs dirs f found path)
  (cond
    [found path]
    [(empty? dirs) '()]
    [else
     (append (find-accum
              (first dirs)f
              #false
              (append path (list (dir-name (first dirs)))))
             (find-in-dirs
              (rest dirs)
              f
              #false
              (list (dir-name (first dirs)))))]))
; Returns the list of filenames for the given directory

(check-expect (dir-filenames DEV-NULL) '())
(check-expect (dir-filenames A-DT) '("read!"))

(define (dir-filenames dt)
  (foldr (lambda (f fnames)
           (cons (file-name f) fnames))
         '()
         (dir-files dt)))