;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname insertion_sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sort-num : (listof num) -> (sorted listof num)
(define (sort-num lst)
  (local 
   ( (define (sort sl ul)
       (cond
         [(empty? ul) sl]
         [(cons? ul) (sort (insert (car ul) sl) (cdr ul))])) )
    (sort '() lst)))


;; insert : num (sorted listof num) -> (sorted listof num)
(define (insert n lst)
  (cond 
    [(empty? lst) (list n)]
    [(cons? lst) 
     (if (<= n (car lst))
         (cons n lst)
         (cons (car lst) (insert n (cdr lst))))]))
                 
                 
                 
