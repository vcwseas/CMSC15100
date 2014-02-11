;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Victor Cheung CMSC 15100 438902 John Reppy
(require 2htdp/image)

;;---------------------------problem 1-------------------------------

;;(make-vec3 x y z) creates a vector structure where
;; - x represents the x component
;; - y represents the y component
;; - z represents the z component
(define-struct vec3 (x y z))


;;vec3-negate: vec3 -> vec3
;;negates all components of the vector,
;;(define vec3-negate (lambda (vec)...))
(check-expect (vec3-negate (make-vec3 1 2 3)) (make-vec3 -1 -2 -3))
(check-expect (vec3-negate (make-vec3 2 3 4)) (make-vec3 -2 -3 -4))

(define vec3-negate
  (lambda (vec)
    (make-vec3 (- (vec3-x vec)) 
               (- (vec3-y vec)) 
               (- (vec3-z vec)))))

;;vec3-add: vec3 vec3 -> vec3
;;add two vectors for resultant vector
;;(define vec3-add (lambda (vec1 vec2)...))
(check-expect (vec3-add (make-vec3 1 2 3) (make-vec3 2 3 4)) (make-vec3 3 5 7))
(check-expect (vec3-add (make-vec3 10 11 12) (make-vec3 -1 -2 0)) (make-vec3 9 9 12))

(define vec3-add 
  (lambda (vec1 vec2)
    (make-vec3 (+ (vec3-x vec1) (vec3-x vec2))
               (+ (vec3-y vec1) (vec3-y vec2))
               (+ (vec3-z vec1) (vec3-z vec2)))))

;;vec3-sub: vec3 vec3 -> vec3
;;substracts the second vector from the first for resultant vector
;;(define vec3-sub (lambda (vec1 vec2)..))
(check-expect (vec3-sub (make-vec3 3 3 3) (make-vec3 2 2 2)) (make-vec3 1 1 1))
(check-expect (vec3-sub (make-vec3 -2 3 10) (make-vec3 1 0 -1)) (make-vec3 -3 3 11))

(define vec3-sub
  (lambda (vec1 vec2)
    (make-vec3 (- (vec3-x vec1) (vec3-x vec2))
               (- (vec3-y vec1) (vec3-y vec2))
               (- (vec3-z vec1) (vec3-z vec2)))))

;;vec3-scale: num vec3-> vec3
;;scales all components by some number
;;(define vec3-scale (lambda (n vec) ...))
(check-expect (vec3-scale 3 (make-vec3 1 1 1)) (make-vec3 3 3 3))
(check-expect (vec3-scale 10 (make-vec3 -1 0 1)) (make-vec3 -10 0 10))

(define vec3-scale
  (lambda (n vec)
     (make-vec3 (* n (vec3-x vec))
                (* n (vec3-y vec))
                (* n (vec3-z vec)))))

;;vec3-dot: vec3 vec3 -> num
;;finds the dot product of the two vectors given
;;(define vec3-dot (lambda (vec1 vec2)...))
(check-expect (vec3-dot (make-vec3 1 1 1) (make-vec3 1 1 1)) 3)
(check-expect (vec3-dot (make-vec3 2 2 2) (make-vec3 -1 0 1)) 0)

(define vec3-dot
  (lambda (vec1 vec2)
    (+ (* (vec3-x vec1) (vec3-x vec2))
       (* (vec3-y vec1) (vec3-y vec2))
       (* (vec3-z vec1) (vec3-z vec2)))))

;;vec3-mag: vec3 -> num
;;finds the magnitude of a vector
;;(define vec3-mag (lambda (vec) ...))
(check-within (vec3-mag (make-vec3 1 1 1)) (sqrt 3) 0.0000001)
(check-within (vec3-mag (make-vec3 -1 0 1)) (sqrt 2) 0.0000001)

(define vec3-mag
  (lambda (vec)
    (sqrt (+ (sqr (abs (vec3-x vec))) 
             (sqr (abs (vec3-y vec)))
             (sqr (abs (vec3-z vec)))))))

;;vec3-norm vec3 -> vector
;;returns a normalized vector
;;(define vec3-norm (lambda (vec) ...))
(check-within (vec3-mag (vec3-norm (make-vec3 2 2 2))) 1 0.000001)
(check-within (vec3-mag (vec3-norm (make-vec3 -95 1000 0.9586))) 1 0.000001)

(define vec3-norm 
  (lambda (vec)
    (make-vec3 (/ (vec3-x vec) (vec3-mag vec))
               (/ (vec3-y vec) (vec3-mag vec))
               (/ (vec3-z vec) (vec3-mag vec)))))

;;---------------------------problem 2-------------------------------

;;halves: (listof num) -> (listof num)
;;divides all numbers in list by two
;;(define halves (lambda (l) ...))
(check-expect (halves '(2 2 2)) '(1 1 1))
(check-expect (halves '(10 -4 8)) '(5 -2 4))

(define halves
  (lambda (l)
    (map (lambda (num) (/ num 2)) l)))

;;multiply-by: num (listof num) -> (listof num)
;;multipies all numbers by a given scalar
;;(define multiply-by (lambda (num l) ... ))
(check-expect (multiply-by 2 '(1 1 2)) '(2 2 4))
(check-expect (multiply-by 2 '(-1 0 2)) '(-2 0 4))

(define multiply-by
  (lambda (num l)
    (map (lambda (n) (* num n)) l)))

;;negatives? :(listof num) -> (listof num)
;;keep negative numbers, discard others
;;(define negatives? (lambda (l) ...))
(check-expect (negatives? '(-1 -2 -3 4 5)) '(-1 -2 -3))
(check-expect (negatives? '(0 908 -334 43 0.12512)) '(-334))

(define negatives?
  (lambda (l)
    (filter (lambda (x) (> 0 x)) l)))

;;larger-than: num (listof num) -> (listof num)
;;keep numbers above threshold, discard other
;;(define larger-than (lambda (num l) ...))
(check-expect (larger-than 7 (list 6 7 8 9 8 7 6)) '(8 9 8))
(check-expect (larger-than 7 (list -123 0.2412 4/5 9.2 81234 -2 pi)) '(9.2 81234))

(define larger-than
  (lambda (num l)
    (filter (lambda (x) (< num x)) l)))

;;taller-than: num (listof image) -> (listof image)
;;keep images taller than given threshold, discard others
;;(define taller-than (lambda (num l-img) ...))
(check-expect (taller-than 5 (list (rectangle 10 10 "solid" "blue") 
                                   (rectangle 1 1 "solid" "blue")))
              (cons (rectangle 10 10 "solid" "blue") empty))
(check-expect (taller-than 50 (list (rectangle 100 120 "solid" "blue") 
                                   (rectangle 1 2 "solid" "blue")
                                   (ellipse 30 40 "solid" "orange")))
              (cons (rectangle 100 120 "solid" "blue") empty))
                           
(define taller-than
  (lambda (num l-img)
    (filter (lambda (x) (< num (image-height x))) l-img)))

;;list-product; (listof num) -> num
;;computer the product of all numbers in the list
;;could've been done with foldr or foldl as well
;;(define list-product (lambda (l) ...))
(check-expect (list-product '(1 2 3 4)) 24)
(check-expect (list-product '(10 20 -3 0.5)) -300)

(define list-product
  (lambda (l)
    (apply * l)))

;;list-xor: (listof bool) -> bool
;;returns true if exactly one item in the list is true, false otherwise
;;(define list-xor (lambda (l) ...))
(check-expect (list-xor (list true false false)) true)
(check-expect (list-xor (list false true true true false)) false)
(check-expect (list-xor '()) false)

(define list-xor
  (lambda (l)
    (local ((define lst-of-true (filter (lambda (n) n) l)))
      (if (= (length lst-of-true) 1)
          true
          false))))
      
;;tower: (listof image) -> image
;;stack all images on top of one another, with the first image at the top
;;second image under that, and so on. 
;;using built in higher order function foldr, which is awesome.
;;(define tower (lambda (l)...))
(check-expect (tower (list(rectangle 10 10 "solid" "red")
                       (rectangle 10 10 "solid" "blue")))
                (local ((define rec1 (rectangle 10 10 "solid" "red"))
                        (define rec2 (rectangle 10 10 "solid" "blue")))
                  (above rec1 rec2)))
(check-expect (tower '()) empty-image)

(define tower
  (lambda (l)
    (foldr above empty-image l)))


;;that-many: (listof num) -> (listof (listof num))
;;builds a list of lists containing "that many" of each
;;(define that-many (lambda (l) ...))
(check-expect (that-many '( 1 2 3)) (list (list 1) (list 2 2) (list 3 3 3)))
(check-expect (that-many '()) empty)

(define that-many 
  (lambda (l)
    (map (lambda (n) (make-list n n)) l)))



;;to Grader: it'd be awesome if you could
;;let me know how you write l-xor cleaner 
;;or simpler, say using foldr. 