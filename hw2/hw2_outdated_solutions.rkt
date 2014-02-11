;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw2_outdated_solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;hw2 artifacts
(require 2htdp/image)

;;that-many: (listof num) -> (listof (listof num))
;;builds a list of lists containing "that many" of each
;;(define that-many (lambda (l) ...))
(check-expect (that-many '( 1 2 3)) (list (list 1) (list 2 2) (list 3 3 3)))
(define that-many 
  (lambda (l)
    (map build-list-of-that-many l)))


;;building-list-of-that-many: num -> (listof num)
;;builds a list with n occurences of n
;;auxiliary function for that-many
;;(define build-list-of-that-many (lambda (n) ...))
(define build-list-of-that-many
  (lambda (n)
    (build-list-of-that-many-aux n 0)))


;;build-list-of-that-many: num num -> (listof num)
;;builds a list with n occurences of n
;;auxiliary function for build-list-of-that-many
;;(define build-list-of-that-many-aux (lambda (n counter)...))
(define build-list-of-that-many-aux
  (lambda (n counter)
    (if (= n counter)
        empty
        (cons n (build-list-of-that-many-aux n (add1 counter))))))

;;-----------------------------------------------------------------

;;tower-tail-recursive : (listof image) -> image
;;stack all images on top of one another, with the first image at the top
;;second image under that, and so on. 
;;This version calls on an auxiliary function that's tail recursive and so
;;should be more space efficient than tower. 
;;(define tower-tail-recursive (lambda (l) ...))
;;eyeball tested
(define tower-tail-recursive
  (lambda (l)
    (tower-aux empty-image l)))

;;tower-aux: image (listof image) -> image
;;stack all images on top of one another, with the first image at the top
;;second image under that, and so on. 
;;auxiliary function for tower-tail-recursive
;;(define tower-aux (lambda (l) ...))
(define tower-aux
  (lambda (img l)
    (if (eqv? (cdr l) empty)
        (above img
               (car l))
        (tower-aux (above img
                          (car l)) (cdr l)))))


;;------------------------------------------------------------------
;;l-xor: num (listof bool) -> bool
;;returns true if exactly one item in the list is true, false otherwise
;;auxiliary function for list-xor
;;does all of the grunt work; never gets called on by user :(
;;can't actually deal with empty lists. 
;;(define l-xor (lambda (l) ...))
(check-expect (l-xor 0 '()) false)
(define l-xor
  (lambda (num l)
    (if (or (> num 1) (eqv? (cdr l) empty))
        (if (= num 1) true false)
        (if (car l) (l-xor (add1 num) (cdr l)) (l-xor num (cdr l))))))

