;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Binary-search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; binary-search : (sorted-vector num) num -> bool
;; return true if the number x is in the vector v,
;; where v is sorted in increasing order.

(define (binary-search v n)
  (local { (define (search lo hi)
             (local { (define mid (floor (/ (+ hi lo) 2))) }
               (cond
                 [(< hi lo) #f]
                 [(= n (vector-ref v mid)) true]
                 [(< n (vector-ref v mid)) (search lo (sub1 mid))]
                 [else (search (add1 mid) hi)]))) }
    (search 0 (- (vector-length v) 1))))


(define test-v (vector 2 5 6 7 7 9 11))
(check-expect (binary-search test-v 0) #f)
(check-expect (binary-search test-v 2) #t)
(check-expect (binary-search test-v 3) #f)
(check-expect (binary-search test-v 7) #t)
(check-expect (binary-search test-v 12) #f)
(check-expect (binary-search (vector 42) 42) #t)
(check-expect (binary-search (vector 42) 17) #f)