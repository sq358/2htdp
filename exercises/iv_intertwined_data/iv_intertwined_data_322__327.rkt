;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iv_intertwined_data_322__327) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define tree0 NONE)

(define tree1 (make-node
               15
               'd
               NONE
               (make-node
                24 'i NONE NONE)))

(define tree2 (make-node
               15
               'd
               (make-node
                87 'h NONE NONE)
               NONE))

(define tree3 (make-node
               10
               'd
               (make-node
                9 'h (make-node
                      5
                      'p
                      NONE NONE)
                (make-node
                 4
                 'u
                 NONE NONE))
               (make-node
                15
                'd NONE NONE)))
  
; Exercise 322.

; Number BT -> Boolean
; Determines if the given number occurs in the given
; binary tree

(check-expect (contains-bt? 1 tree0) #false)
(check-expect (contains-bt? 10 tree1) #false)
(check-expect (contains-bt? 87 tree2) #true)

(define (contains-bt? n bt)
  (cond
    [(no-info? bt) #false]
    [else
     (or (= (node-ssn bt) n)
         (contains-bt? n (node-left bt))
         (contains-bt? n (node-right bt)))]))

; Exercise 323.

; BTSearchResult is one of
; - #false
; - Symbol

; Number BT -> BTSearchResult
; Searches the given binary tree for n and if found
; returns the corresponding node's name or false if
; number not found in the tree

(check-expect (search-bt 10 NONE) #false)
(check-expect (search-bt 10 tree1) #false)
(check-expect (search-bt 87 tree2) 'h)

(define (search-bt n bt)
  (cond
    [(no-info? bt) #false]
    [(= (node-ssn bt) n) (node-name bt)]
    [else
     (if (boolean? (search-bt n (node-left bt)))
         (search-bt n (node-right bt))
         (search-bt n (node-left bt)))]))

; Exercise 324.

; BT -> [List-of Number]
; For the given binary tree, produces the sequence
; of node ssn numbers as they show up in the tree from
; left to right

(check-expect (inorder tree0) '())
(check-expect (inorder tree1) '(15 24))
(check-expect (inorder tree2) '(87 15))
(check-expect (inorder tree3) '(5 9 4 10 15))

(define (inorder bt)
  (cond
    [(no-info? bt)'()]
    [else
     (append (inorder (node-left bt))
             (list (node-ssn bt))
             (inorder (node-right bt)))]))

;; Q. What does inorder produce for a binary search tree?
;; A. Lisf of SSNs in ascending order

; Exercise 325.

; BSTSearchResult is one of:
; - NONE
; - String

; Number BST -> BSTSearchResult
; Search the given n in the given BST

(check-expect (search-bst 10 NONE) NONE)
(check-expect (search-bst 15 tree1) 'd)
(check-expect (search-bst 14 tree1) NONE)
(check-expect (search-bst 5 tree3) 'p)

(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [(= n (node-ssn bst)) (node-name bst)]
    [else
     (if (< n (node-ssn bst))
         (search-bst n (node-left bst))
         (search-bst n (node-right bst)))]))

; Exercise 326.

; BST Number Symbol -> BST
; Inserts the node with numnber n and symbol s into the
; given bst

(check-expect (create-bst NONE 10 's)
              (make-node 10 's NONE NONE))

(check-expect (create-bst tree1 10 's)
              (make-node
               15
               'd
               (make-node 10 's NONE NONE)
               (make-node
                24 'i NONE NONE)))

(check-expect (create-bst tree3 3 'v)
              (make-node
               10
               'd
               (make-node
                9
                'h
                (make-node
                 5
                 'p
                 (make-node
                  3
                  'v
                  (make-no-info)
                  (make-no-info))
                 (make-no-info))
                (make-node
                 4
                 'u
                 (make-no-info)
                 (make-no-info)))
               (make-node
                15
                'd
                (make-no-info)
                (make-no-info))))
              
(define (create-bst b n sy)
  (cond
    [(no-info? b) (make-node n sy NONE NONE)]
    [else
     (if (< n (node-ssn b))
         (make-node (node-ssn b) (node-name b)
                    (create-bst (node-left b) n sy)
                    (node-right b))
         (make-node (node-ssn b) (node-name b)
                    (node-left b)
                    (create-bst (node-right b) n sy)))]))

; Exercise 327.

; BST [List-of [List Number Symbol]] -> BST
; For the given list of numbers and names and the bst
; produces a bst

(define bst-value-sample
  '((99 o)
    (77 l)
    (24 i)
    (10 h)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))

(check-expect (create-bst-from-list NONE '((99 o)))
              (make-node 99 'o NONE NONE))

(check-expect (create-bst-from-list
               (make-node 99 'o (make-no-info) (make-no-info))
               '((77 l)))
              (make-node
               99
               'o
               (make-node
                77
                'l
                (make-no-info)
                (make-no-info))
               (make-no-info)))

(define (create-bst-from-list bst a-list)
  (foldr (lambda (val b)
           (create-bst b (first val) (second val)))
         bst
         a-list))

(create-bst-from-list NONE bst-value-sample)
(inorder mytree)