#lang racket
(require racket/match)

;; research: item num num vector -> num
;; given an item, finds the index of the item
;; if it's in the vector, -1 otherwise.
(define (research x v)
  (local { (define (aux i j)(cond
                            [(> i j) -1]
                            [(= i j) (if (= x (vector-ref v i))
                                         i
                                         -1)]
                            [else (match (floor (/ (+ i j) 2))
                                    [mid (cond
                                           [(<= x (vector-ref v mid))
                                            (match (aux (add1 mid) j)
                                              [-1 (aux i mid)]
                                              [else -1])]
                                           [else (match (aux i mid)
                                                   [-1 (aux (add1 mid) j)]
                                                   [else -1])])])])) }
    
    (aux 0 (sub1 (vector-length v)))))
(define test-v (list->vector (sort (build-list 1000000 (λ (n) (random 1000000))) < )))