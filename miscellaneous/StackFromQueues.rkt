#lang racket
;; a queue is a list. 

;; pop : queue -> num queue
(define (pop q)
  (values (car q) (cdr q)))

;; put: queue n -> queue
(define (put q n)
  (append q (list n)))


  