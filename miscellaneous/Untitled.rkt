#lang Racket

(define-struct pair (a b))


;; pack : (listof Ω) -> (listof (listof Ω))
;; packs consecutive duplicates of list elements
;; into sublists
(define (pack lst)
  (local { (define (p-aux lst acc)
             (if (empty? lst) (make-pair lst acc)
                 (if (equal? (car lst) (car acc))
                     (p-aux (cdr lst) (cons (car lst) acc))
                     (make-pair lst acc)))) 
           
           (define (p-aux2 lst acc)
             (if (empty? lst) acc
                 (match (p-aux (cdr lst) (list (car lst)))
                   [(pair a b)
                    (p-aux2 a (cons b acc))])) ) }
    
    (reverse (p-aux2 lst '()))))
                  
           
;; pack-2 : (listof Ω) -> (listof (listof Ω))
;; l - list
;; ps - packed subset

(define (pack-2 lst)
  (let lp ([l (cdr lst)]
           [ps (list (list (car lst)))])
    (cond
      [(empty? l) ps]
      [(equal? (car l) (car (car ps)))
       (lp (cdr l) (list (cons (car l) ps)))
       (lp (cdr l) (cons (list (car l)) ps))])))
        



 (define pack-3 
   (lambda (xs) 
     (if (null? xs) '() 
       (let loop ((rest (cdr xs)) 
                  (pkg (list (car xs)))) 
         (cond ((null? rest) 
                (list pkg)) 
               ((eq? (car pkg) (car rest)) 
                (loop (cdr rest) (cons (car rest) pkg))) 
               (else 
                 (cons pkg (loop (cdr rest) (list (car rest)))))))))) 

  