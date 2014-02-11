;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname BST_tree_traversals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; tree traversals
(require racket/match)
(define-struct bst (item lsub rsub))
(define-struct pair (left right))

(define bst-1 (make-bst 5 
                        (make-bst 3 
                                  empty
                                  (make-bst 4 empty empty)) 
                        (make-bst 6 empty 
                                  (make-bst 8 (make-bst 7 empty empty) (make-bst 9 empty empty)))))

(define test-bst (make-bst 5 
                           (make-bst 3 (make-bst 2 empty empty) (make-bst 4 empty empty)) 
                           (make-bst 8 (make-bst 7 (make-bst 6 empty empty) empty) (make-bst 10 empty (make-bst 11 empty empty)))))

;; lr-post-traversal : bst -> (listof item)
;; left-right post-order traversal
(define (lr-po-traversal t)
(cond
  [(empty? t) empty]
  [else (append (lr-po-traversal (bst-lsub t)) (lr-po-traversal (bst-rsub t)) (list (bst-item t)))]))

;; rl-post-traversal : bst -> (listof item)
;; right-left post-order traveral
(define (rl-po-traversal t)
  (cond
    [(empty? t) empty]
    [else (append (rl-po-traversal (bst-rsub t)) (rl-po-traversal (bst-lsub t)) (list (bst-item t)))]))

;; lr-pre-traversal : bst -> (listof item)
;; left-right pre-order traversal
(define (lr-pre-traversal t)
  (cond
    [(empty? t) empty]
    [else (append (list (bst-item t)) (lr-pre-traversal (bst-lsub t)) (lr-pre-traversal (bst-rsub t)))]))

;; rl-pre-traversal : bst -> (listof item)
(define (rl-pre-traversal t)
  (cond
    [(empty? t) empty]
    [else (append (list (bst-item t)) (rl-pre-traversal (bst-rsub t)) (rl-pre-traversal (bst-lsub t)))]))

;; lr-io-traversal : bst -> (listof item)
(define (lr-io-traversal t)
  (cond
    [(empty? t) empty]
    [else (append (lr-io-traversal (bst-lsub t)) (list (bst-item t)) (lr-io-traversal (bst-rsub t)))]))

;; rl-io-traversal : bst -> (listof item)
(define (rl-io-traversal t)
  (cond
    [(empty? t) empty]
    [else (append (rl-io-traversal (bst-rsub t)) (list (bst-item t)) (rl-io-traversal (bst-lsub t)) )]))
                              
;; lr-lo-traversal : bst -> (listof item)
;; left to right level order traversal
 (define (lr-lo-traversal t)
   (local { (define (llt Q lst)
              (if (empty? Q)
                  lst
              (llt (append (cdr Q) (next (car Q))) (append lst (list (bst-item (car Q))))))) }
     (llt (list t) empty)))
 
 
;; next: bst -> (listof bst)
;; view the list of nodes avalible from the current node. 
 (define (next t)
   (cond
     [(and (empty? (bst-lsub t)) (empty? (bst-rsub t))) empty]
     [(empty? (bst-lsub t)) (list (bst-rsub t))]
     [(empty? (bst-rsub t)) (list (bst-lsub t))]
     [else (list (bst-lsub t) (bst-rsub t))]))


 
 ;; bt-search : bst (alpha -> bool)  -> bool
 ;; returns true if an item exists such that 
 ;; pred evaluates to true.
 (define (bt-search t pred)
   (local { (define (search t)
            (match t
              ['() #f]
              [(bst item lsub rsub)
               (if (pred item)
                   #t
                   (match (search lsub)
                     [#f (search rsub)]
                     [#t #t]))]))}
     (search t)))
             
 
 
 (define (split-min t) 
  (match t 
    [(bst r '() rsub) (make-pair r rsub)] 
    [(bst r lsub rsub) 
     (match (split-min lsub) 
         [(pair m lsub*) (make-pair m (make-bst r lsub* rsub))])] 
    ['() (error 'split-min "empty")]))
 
 ;; dfs-list: graph -> (list node)
;; return a list of nodes in DFS order
(define (dfs-list g)
  (local { (define (dfs nd nds)
             (cond 
               [(member nd nds) nds]
               [else (foldl dfs (cons nd nds) (succs nd))])) }
        (reverse (foldl (λ (nd nds) (dfs (node-name nd) visited)) '() g))))