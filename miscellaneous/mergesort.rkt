;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; mergesort: (listof num) -> (listof num)
;; mergesorts a list of numbers
(define (mergesort lst)
  (local { (define losub (map list lst))
           (define (repeat-merge alon)
                   (if (empty? (cdr alon)) 
                       (car alon)
                       (repeat-merge (merge alon)))) }
    (repeat-merge losub)))
      

;; merge : (list (listof num) ...) -> (list (listof num) ...)
;; result of merge is that the number of sublists 
;; is halved and merged lists are in order.
(define (merge lol)
  (cond
    [(empty? lol) empty]
    [(empty? (cdr lol)) lol]
    [else (cons (ms (car lol) (cadr lol)) (merge (cddr lol)))]))
      
       
      
;; ms : (listof num) (listof num) -> (listof num)
;; merges two lists such that the resulting list is sorted.
(define (ms fl sl)
                 (if (or (empty? fl) (empty? sl))
                     (append fl sl)
                     (if (< (car fl) (car sl))
                         (cons (car fl) (ms (cdr fl) sl))
                         (cons (car sl) (ms fl (cdr sl))))))


