;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vector-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; natfold: (nat X -> X) X nat -> X
;; like foldl
(define (natfold func z n)
  (local { (define (fold i acc)
           (cond
             [(< i n) (fold (add1 i) (func i acc))]
             [else acc]))}
    (fold 0 z)))

;; vec-sum: (vectorof num) -> num
;; sums a vector
(define (vec-sum v)
  (natfold (λ (i sum) (+ (vector-ref v i) sum)) 0 (vector-length v)))

;; vec->list (vectorof X) -> (listof X)
;; turns a vector into a list
(define (vec->list v)
  (reverse (natfold (λ (i lst) (cons (vector-ref v i) lst)) '() (vector-length v))))

;; list->vec: (listof X) -> (vec X)
;; turns a list into a vector
(define (list->vec lst)
  (build-vector (length lst) (λ (i) (list-ref lst i))))

;; vec-max: (vec num) -> num
;; finds the maximum in a vector
(define (vec-max v)
  (natfold (λ (i max) (if (> (vector-ref v i) max)
                          (vector-ref v i)
                          max))
           (vector-ref v 0)
           (vector-length v)))

;; vec-reduce: (alpha alpha -> alpha) alpha (vec alpha) -> alpha
(define (vec-reduce func a v)
  (natfold (λ (i acc) (func (vector-ref v i) acc)) a (vector-length v)))

;; vec-indices : (alpha -> bool) (vec alpha) -> (listof num)
(define (vec-indices pred v)
  (natfold (λ (i acc) (if (pred (vector-ref v i))
                          (cons i acc)
                          acc)) '() (vector-length v)))

;; vec-filter: (alpha -> bool) (vec alpha) -> (vec alpha)
(define (vec-filter pred v)
  (match (vec-indices pred v)
    ['() (vector)]
    [lst (list->vec (natfold (λ (i acc) (if (member i lst)
                                 (cons (vector-ref v i) acc)
                                 acc)) '() (vector-length v)))]))
                                 
;; vec-reverse : (vec X) -> void
(define (vec-reverse v)
  (natfold (λ (i j) (begin 
                        (swap v i j)
                        (sub1 j))) (sub1 (vector-length v)) (floor (/ (vector-length v) 2))))
                                
                                          
                                          
;; swap 
(define (swap v i j)
  (match (vector-ref v j)
    [tmp (begin 
           (vector-set! v j (vector-ref v i))
           (vector-set! v i tmp))]))

(define tst-v (vector 1 2 3 4 5 6))9