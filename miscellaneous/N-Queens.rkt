;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname N-Queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Let's try and solve N-Queens



;; board is represented by a list of squares 
;; a square is a (make-square row col)
;; - row is a number < N
;; - col is a number < N
(define-struct square (row col))

;; test board
(define test-board (list (make-square 0 0)
                           (make-square 1 2)))

;; currys a function; THANSK HASKELL
(define (curry2 f x)
  (λ (y) (f x y)))

;; threaten? : square board -> bool
;; tells you whether putting a queen in some square
;; invalidates the board (by putting a queen in danger)
;; true is invalidated
;; false if not (which is good)
(define (threaten? s b)
  (local { (define (square-threaten? s1 s2)
             (or (= (square-row s1) (square-row s2))
                 (= (square-col s1) (square-col s2))
                 (= (abs (- (square-row s1) (square-row s2)))
                    (abs (- (square-col s1) (square-col s2)))))) }
    (ormap (curry2 square-threaten? s) b)))
         

;; N-Queens: num -> (listof board)
;; returns a list of solutions to the N-Queens problem
(define (N-Queens N)
  (local { (define (tryrow r lst-of-board)
             (if (< r N)
                 (local { (define (trycol c board)
                            (cond
                              [(threaten? (make-square r c) board) '()]
                              [else (tryrow (add1 r) (cons (make-square r c) board))])) }
                   (build-list N trycol))
         
                   lst-of-board)) } 
    (tryrow 0 empty)))





;; N-Queens : num -> (listof board)
(define (N-Queens N)
  (local { (define (tryrow r board)
             (if (< r N)
                 (local { (define (trycol c)
                            (if (c < N)
                                  (if (threaten? (make-square r c) board)
                                      #f
                                      (local { (define try (tryrow (add1 r) (cons (make-square r c) board))) }
                                        (if (false? try)
                                            #f
                                            (cons (make-square r c) board))))
                                 #f)) }
                   (map trycol (build-list N identity))