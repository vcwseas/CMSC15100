;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; dataset refers to (listof posn)
(define sample-dset1 (list (make-posn 2 1)
                           (make-posn 5 7)
                           (make-posn 8 9)
                           (make-posn 9 10)))

(define sample-dset2 (list (make-posn 1 1)
                           (make-posn 2 4)
                           (make-posn 3 9)
                           (make-posn 4 16)
                           (make-posn 5 25)
                           (make-posn 6 36)))
                            


;; (make-lineq m b) creates a linear regression equation where
;; - m refer to the slope
;; - b refers to the y intercept
(define-struct lineq (m b))

;; linreg: dataset -> lineq
;; calculates a linear regression equation for some dataset
(check-within (lineq-m (linreg sample-dset1)) 1.23333 0.0001 )
(check-within (lineq-b (linreg sample-dset1)) -0.65 0.0001 )
(check-within (lineq-m (linreg sample-dset2)) 7 0.0001 )
(check-within (lineq-b (linreg sample-dset2)) -9.33333 0.0001 )
(check-error (linreg (list (make-posn 10 10))))

(define linreg
  (λ (dset)
    (if (< (length dset) 2) 
        (error "linear regression meaningless on fewer than two points")
        (make-lineq (slope dset) (intercept dset)))))


;; intercept: dataset -> num
;; finds the y intercept of a linear regression equation for some dataset
(check-within (intercept sample-dset1) -0.65 0.0001)
(check-within (intercept sample-dset2) -9.33333 0.0001)
(define intercept
  (λ (dset)
    (/ (- (* (sum (list-of-posn-y dset)) (sum (list-sqr (list-of-posn-x dset))))
          (* (sum (list-of-posn-x dset)) (sum (list-of-posn-product dset))))
       (- (* (length dset) (sum (list-sqr (list-of-posn-x dset))))
          (expt (sum (list-of-posn-x dset)) 2)))))


;; slope: dataset -> num
;; finds the slope of a linear regression equation for some dataset
(check-within (slope sample-dset1) 1.23333 0.00001)
(check-within (slope sample-dset2) 7 0.00001)
(define slope
  (λ (dset)
    (/ (- (* (length dset) (sum (list-of-posn-product dset)))
          (* (sum (list-of-posn-x dset)) (sum (list-of-posn-y dset))))
       (- (* (length dset) (sum (list-sqr (list-of-posn-x dset))))
          (expt (sum (list-of-posn-x dset)) 2)))))

;; list-sqr: (listof num) -> (listof num)
;; squares every element in the dataset
(check-expect (list-sqr '(1 2 3 4 5)) '(1 4 9 16 25))
(check-expect (list-sqr '(3 4 5 6 -7)) '(9 16 25 36 49))
(define list-sqr
  (λ (lst)
    (map (λ (n) (* n n)) lst)))

;; sum: (listof num) -> num
;; sums every element in the dataset
(check-expect (sum '(1 2 3 4 5 6)) 21)
(check-expect (sum '(1 2 3 4 5 6 7 8 9 10)) 55)
(define sum
  (λ (lst)
    (foldr + 0 lst)))

;; posn-product: posn -> num
;; finds the product of posn-x * posn-y
(check-expect (posn-product (make-posn 4 5)) 20)
(check-expect (posn-product (make-posn 3 52)) 156)
(define posn-product
  (λ (p)
    (* (posn-x p) (posn-y p))))

;; list-of-posn-product: (listof posn) -> (listof num)
;; creates of a list where each element is the product of the
;; x and y values of a corresponding posn
(check-expect (list-of-posn-product (list (make-posn 1 1)
                                          (make-posn 2 2))) '(1 4))
(check-expect (list-of-posn-product (list (make-posn 9 10)
                                          (make-posn 6 8))) '(90 48))
(define list-of-posn-product
  (λ (lst)
    (map posn-product lst)))

;; list-of-posn-x: (listof posn) -> (listof num)
;; creates a list where each element is the posn-x value
;; of a corresponding posn
(check-expect (list-of-posn-x (list (make-posn 1 1)
                                    (make-posn 2 2))) '(1 2))
(check-expect (list-of-posn-x (list (make-posn 958 1)
                                    (make-posn 1/3 2))) '(958 1/3))
(define list-of-posn-x 
  (λ (lst)
    (map (λ (p) (posn-x p)) lst)))

;; list-of-posn-y: (listof posn) -> (listof num)
;; creates a list where each element is the posn-y value
;; of a corresponding posn
(check-expect (list-of-posn-y (list (make-posn 1 1)
                                    (make-posn 2 2))) '(1 2))
(check-expect (list-of-posn-y (list (make-posn 1 -90)
                                    (make-posn 2 1.2312))) '(-90 1.2312))
(define list-of-posn-y 
  (λ (lst)
    (map (λ (p) (posn-y p)) lst)))