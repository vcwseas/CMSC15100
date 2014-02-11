;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 CMSC 15100 John Reppy
(require 2htdp/image)

;;--------------------------Problem 1----------------------------
;; concat-with : string (listof string) -> string
;; concatenates all strings with the given separator
;; returns empty if the (listof string) is empty.
(check-expect (concat-with "," '("hello" "world")) "hello,world")
(check-expect (concat-with " " '("give" 
                                 "me"
                                 "liberty"
                                 "or"
                                 "give"
                                 "me"
                                 "death")) "give me liberty or give me death")
(check-expect (concat-with " " '("this"
                                 "homework"
                                 "took"
                                 "forever"
                                 "to"
                                 "do")) "this homework took forever to do")
(check-expect (concat-with "" '()) '())
(check-expect (concat-with "nonsense" '("Crosby")) "Crosby")

(define concat-with
  (λ (s lst)
    (if (empty? lst) 
        '()
        (if (empty? (cdr lst)) 
            (car lst)
            (string-append (car lst) s (concat-with s (cdr lst)))))))

;;--------------------------Problem 2----------------------------
;; chessboard: num num color color -> image
;; creates a chess board with a set number of squares,
;; a set side length of each sqare,
;; the light square's color
;; the dark square's color. 
;; eyeball-tested
(check-expect (image-width (chessboard 8 50 "gray" "black")) 400)
(check-expect (image-height (chessboard 8 50 "gray" "black")) 400)
(check-error (chessboard 100000 0 "gray" "black"))

(define chessboard
  (λ (num side light dark)
    (if (= side 0) 
        (error "each square must be at least 1 pixel in side length")
        (local ((define darksquare (square side "solid" dark))
                (define lightsquare (square side "solid" light))
                (define light-right-row (row num lightsquare darksquare))
                (define dark-right-row (row num darksquare lightsquare)))
          (stack-rows num dark-right-row light-right-row true)))))

;; stack-rows: num image image bool-> image
;; stacks a given number of rows such that the last row
;; always ends with a light square.
(define stack-rows
  (λ (num dark-right-row light-right-row bool)
       (cond
         [(= num 0) empty-image]
         [bool (above (stack-rows (- num 1) dark-right-row light-right-row false)
                      light-right-row)]
         [(not bool) (above (stack-rows (- num 1) dark-right-row light-right-row true)
                      dark-right-row)])))


;; row : num image image -> image
;; creates a row such that the first parameter is always last
(define row
  (λ (num last-square other-square)
    (if (= num 1)
        last-square
        (if (= (remainder num 2) 1)
            (beside last-square (row (- num 1) last-square other-square))
            (beside other-square (row (- num 1) last-square other-square))))))
        
;;--------------------------Problem 3----------------------------
;; Pascal's Triangle is defined as follows
;; 1 (row 0)
;; 1 1 (row 1)
;; 1 2 1 (row 2)
;; 1 3 3 1 (row 3)
;; 1 4 6 4 1 (row 4)
;; 1 5 10 10 5 1 (row 5)
;; and so on.

;; pascal : num -> (listof (listof num))
;; computes the first n rows of Pascal's triangle,
;; where n is the number of rows. 
(check-expect (pascal 0) empty)
(check-expect (pascal 1) (list (list 1)))
(check-expect (pascal 7) 
              (list
               (list 1)
               (list 1 1)
               (list 1 2 1)
               (list 1 3 3 1)
               (list 1 4 6 4 1)
               (list 1 5 10 10 5 1)
               (list 1 6 15 20 15 6 1)))

(define pascal 
  (λ (n)
    (if (= n 0) 
        empty
        (pascal-aux (- n 1)))))


;; pascal-aux : num -> (listof (listof num))
;; computes the first n+1 rows of Pascal's triangle
;; where n is the number of the final row. 
(check-expect (pascal-aux 6) 
              (list
               (list 1)
               (list 1 1)
               (list 1 2 1)
               (list 1 3 3 1)
               (list 1 4 6 4 1)
               (list 1 5 10 10 5 1)
               (list 1 6 15 20 15 6 1)))

(check-expect (pascal-aux 0) (list (list 1)))

(define pascal-aux
  (λ (r)
    (cond
      [(= r 0) (list '(1))]
      [else (local ((define smaller (pascal-aux (- r 1))))
              (append smaller 
                  (list (tag-one (sum-two (last-of-list smaller))))))])))

;; last-of-list: (listof alpha) -> alpha
;; returns the last element of a list
(check-expect (last-of-list (list 1 2 3 4)) 4)
(check-expect (last-of-list (list 2 -1 3 10)) 10)
(define last-of-list
  (λ (lst)
    (if (empty? (cdr lst)) (car lst)
        (last-of-list (cdr lst)))))

;; tag-one: (listof num) -> (listof num)
;; adds one to both ends of the given list.
(check-expect (tag-one '()) '(1 1))
(check-expect (tag-one '(1 2 3)) '(1 1 2 3 1))
(define tag-one
  (λ (lst)
    (append (cons 1 lst) '(1))))

;; sum-two: (listof num) -> (listof num)
;; creates a list where every element is the sum
;; of two consecutive elements from the given lst,
;; and where an initial list of size 3 would create 
;; a new list of size 2. 
(check-expect (sum-two '(1 3 3 1)) '(4 6 4))
(check-expect (sum-two '(1 4 6 4 1)) '(5 10 10 5))
(check-error (sum-two '()))
(define sum-two
  (λ (lst)
    (cond 
      [(empty? lst) (error "sum-two needs a list of at least one number")]
      [(empty? (cdr lst)) empty]
      [(cons? lst) (cons (+ (car lst) (car (cdr lst))) (sum-two (cdr lst)))])))
     
;;--------------------------Problem 4----------------------------
;; dataset refers to a list of posns.

;; (make-analysis lineq num) creates an analysis structure where
;; - lineq is the equation of a linear regression
;; - num is the value of r^2
(define-struct analysis (lineq r2))

;; full-linreg: dataset -> analysis
;; computes a full linear regression analysis
;; and creates a strucutre analysis with lineq and r^2
(check-within (lineq-m (analysis-lineq (full-linreg sample-dset1))) 1.23333 0.0001 )
(check-within (lineq-b (analysis-lineq (full-linreg sample-dset1))) -0.65 0.0001 )
(check-within (lineq-m (analysis-lineq (full-linreg sample-dset2))) 7 0.0001 )
(check-within (lineq-b (analysis-lineq (full-linreg sample-dset2))) -9.33333 0.0001 )
(check-within (analysis-r2 (full-linreg sample-dset1)) 0.936056 0.0001)
(check-within (analysis-r2 (full-linreg sample-dset2)) 0.958245 0.0001)
(check-error (full-linreg '()))
(define full-linreg 
  (λ (lst)
    (make-analysis (linreg lst) (corr2 lst))))

;; corr2: dataset -> num
;; computes the r^2 value
(check-within (corr2 sample-dset1) 0.936056 0.0001) 
(check-within (corr2 sample-dset2) 0.958245 0.0001)
(define corr2
  (λ (lst)
    (sqr (corr lst))))


;; corr: dataset -> num
;; finds the linear correlation coefficient r
(check-within (corr sample-dset1) 0.967506 0.0001)
(check-within (corr sample-dset2) 0.9789 0.0001)
(define corr
  (λ (lst)
    (/ (- (average (list-of-posn-product lst)) (* (average (list-of-posn-x lst))
                                                  (average (list-of-posn-y lst))))
       (* (stdev (list-of-posn-x lst)) (stdev (list-of-posn-y lst))))))


;; average: (listof num) -> num
;; finds the arithmetic mean of a list of numbers
(check-expect (average (list 1 2 3)) 2)
(check-expect (average (list 10 10 10)) 10)
(define average
  (λ(lst)
    (/ (sum lst) (length lst))))

;; stdev: (listof num) -> num
;; finds the standard deviation of a list of numbers
(check-within (stdev '(1 2 3 4 5)) 1.41421 0.00001)
(check-within (stdev '(12 32 43 5 21)) 13.63231 0.00001)
(define stdev
  (λ (lst)
    (sqrt (variance lst))))


;; variance: (listof num) -> num
;; finds the variance of a list of numbers
(check-expect (variance '(1 2 3 4 5)) 2)
(check-within (variance '(12 32 43 5 21)) 185.84 0.00001)
(define variance
  (λ (lst)
    (/ (sum (list-sqr (list-of-diff-of-mean lst)))
       (length lst))))

;; list-of-diff-of-mean: (listof num) -> (listof num)
;; builds a list of differences between the elements 
;; of that list and the mean of the list.
(check-expect (list-of-diff-of-mean (list 1 2 3)) '(-1 0 1))
(check-expect (list-of-diff-of-mean (list 0 0 0)) '(0 0 0))
(check-expect (list-of-diff-of-mean (list -1 0 1)) '(-1 0 1))
(define list-of-diff-of-mean
  (λ (lst)
    (local ((define mean (average lst)))
      (map (lambda (n) (- n mean)) lst))))


;;--------------------------Lab3---------------------------------
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
(check-error (linreg (list (make-posn 1 1))))

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


;;--------------------------problem5---------------------------------
;; design : num -> img
;; draws square design as displayed in hw3 with given side length
(define (design s)
  (overlay 
   (square s "outline" "maroon")
   (above
    (beside (rectangle (/ s 2) 1 "solid" "darkgray")
            (overlay 
             (circle (/ s 6) "solid" "gray")
             (circle (/ s 5) "solid" "maroon")))
    (rectangle 1 (/ s 2) "solid" "darkgray"))
   (square s "solid" "darkgray")))

;; recursive-tiles: num -> img
;; creates a recurisve tile design
(check-expect (image-width (recursive-tiles 512)) 512)
(check-expect (image-height (recursive-tiles 512)) 512)
(check-expect (image-width (recursive-tiles 30)) 30)
(check-expect (image-height (recursive-tiles 30)) 30)
(check-error (recursive-tiles 0))

(define recursive-tiles
  (λ (side) 
    (local ( (define topright-square (overlay/align "left" "bottom" 
                                                    (topright (/ side 4))
                                                    (square (/ side 2) "solid" "maroon")))
             (define topleft-square (flip-horizontal topright-square))
             (define bottomrec (flip-vertical (beside topleft-square topright-square))))
      (if (>= side 4)
      (above (beside topleft-square topright-square)
             bottomrec)
      (error "recursive-tiles cannot have side length < 4 since design does not take
              arguments less than 2")))))
      
;; topright-up: num -> image
;; creates a vertical portion of the recursive-tiles image
;; for the topright quarter; auxiliary to recursive-tiles.
;; eyeball tested.
(define topright-up
  (λ (side)
    (cond
      [(<= side 2) (design 2)]
      [else (above (beside (topright-up (/ side 2)) (topright-up (/ side 2)))
                   (design side))])))

;; topright-right: num -> image
;; creates a horizontal portion of the recursive-tiles image
;; for the topright quarter; auxiliary to recursive-tiles.
;; eyeball tested.
(define topright-right
  (λ (side)
    (cond
      [(<= side 2) (design 2)]
      [else (beside (design side)
                    (above (topright-right (/ side 2)) 
                           (topright-right (/ side 2))))])))
;; topright: num -> image
;; creates the topright quarter of the recursive-tiles image
;; auxiliary to recursive-tiles.
;; eyeball tested.
(define topright
  (λ (side)
    (cond
      [(<= side 2) (overlay/align "left" "bottom"
                                  (topright-up side)
                                  (topright-right side))]
      [else (overlay/offset
             (overlay/align "left" "bottom"
                            (topright-up side)
                            (topright-right side))
             (/ side 2)
             (- (/ side 2))
             (topright (/ side 2)))])))
 
;; tests for time to process recursive-tiles
;;(time (recursive-tiles 512))
;;(time (recursive-tiles 1024))

;; eyeball tests for chessboard
;;(chessboard 8 50 "black" "gray")
;;(chessboard 3 50 "black" "gray")