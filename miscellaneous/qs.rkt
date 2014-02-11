;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname qs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (partition pred lst)
  (list (filter pred lst) (filter (lambda (n) (not (pred n))) lst) ))


(define (qs lst)
  (if (empty?  lst)
      empty
      (local {(define pivot (car lst))
              (define part (partition (lambda (n) (<= n pivot)) (cdr lst)))}
        (append (qs (car part)) (list pivot) (qs (cadr part))))))
