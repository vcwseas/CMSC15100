;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname other_implementations_of_riemann) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; t-riemann/lr : (num -> num) num num num -> num
;; tail-recursive
(define (t-riemann/lr func l r w)
  (local (
          (define (r/lr func l r w sum)
             (local ( (define l+w (+ l w)) )
             (cond
               [(< l+w (+ r w)) 
                (r/lr func l+w r w (+ sum (* w (func l+w))))]
               [else sum]))) )
    (r/lr func l r w 0)))

;; riemann/lr : (num -> num) num num num -> num
;; not tail-recursive
(define (riemann/lr func l r w)
  (cond
    [(< (+ l w) (+ r w)) (+ (* (func (+ l w)) w) 
                            (riemann/lr func (+ l w) r w))]
    [else 0]))