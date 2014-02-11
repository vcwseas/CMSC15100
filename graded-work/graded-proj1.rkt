;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graded-proj1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 CMSC 15100 John Reppy
(require 2htdp/image)
(require racket/match)

;;---------------------------------------------------------------
;;-------------------------Definitions---------------------------
;;---------------------------------------------------------------

;; a size is either 1, 2, or 3,
;; representing small, medium, large in that order

;; a player is either 'blue or 'orange

;; a square is a natural number between 0 and 8 inclusive

;; a piece is a (make-piece s p) where
;; - s is a size, and
;; - p is a player
(define-struct piece (size player))

;; an intro is a (make-intro p s) where
;; - p is a piece, and
;; - s is a square
(define-struct intro (piece square))

;; a shift is a (make-shift src dst) where
;; - src and dst are both squares
(define-struct shift (src dst))

;; a move is either
;; - an intro, or
;; - a shift

;; an inventory is a (listof piece)

;; a board is a (listof (listof piece)) of length exactly 9
;; representing the squares of the board.  Each square is represented
;; by a list of pieces, where the pieces are ordered from outermost
;; (i.e., biggest) to innermost (smallest).  An square with no pieces
;; is represented by the empty list.

;; The order of the 9 items in the list corresponds to the 
;; squares on the board as follows:
;; 0 1 2
;; 3 4 5
;; 6 7 8

;; a game is a (make-game next oinv binv board) where
;; - next is a player, 
;; - oinv ("orange inventory") is an inventory,
;; - binv ("blue inventory") is an inventory, and
;; - board is a board (per definition above)
(define-struct game (next oinv binv board))

;;---------------------------------------------------------------
;;----------------------GameLogic--------------------------------
;;---------------------------------------------------------------

;; new-game : player -> game
;; generates a game in the initial state with player as the first player.
(check-expect (new-game 'blue)
              (make-game 'blue
                         (list (make-piece 1 'orange)
                                (make-piece 1 'orange)
                                (make-piece 2 'orange)
                                (make-piece 2 'orange)
                                (make-piece 3 'orange)
                                (make-piece 3 'orange))
                          (list (make-piece 1 'blue)
                                (make-piece 1 'blue)
                                (make-piece 2 'blue)
                                (make-piece 2 'blue)
                                (make-piece 3 'blue)
                                (make-piece 3 'blue))
                          (list empty empty empty empty
                                empty empty empty empty
                                empty)))

(check-expect (new-game 'orange)
              (make-game 'orange
                         (list (make-piece 1 'orange)
                                (make-piece 1 'orange)
                                (make-piece 2 'orange)
                                (make-piece 2 'orange)
                                (make-piece 3 'orange)
                                (make-piece 3 'orange))
                          (list (make-piece 1 'blue)
                                (make-piece 1 'blue)
                                (make-piece 2 'blue)
                                (make-piece 2 'blue)
                                (make-piece 3 'blue)
                                (make-piece 3 'blue))
                          (list empty empty empty empty
                                empty empty empty empty
                                empty)))


(define (new-game firstplayer)
  (make-game firstplayer
             (list (make-piece 1 'orange)
                   (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list empty empty empty empty
                   empty empty empty empty
                   empty)))

;; pieces-at : board int -> (listof piece)
;; Returns the list of (from zero to three) pieces at the given square. 
(check-expect (pieces-at (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 3 'blue) (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)
           2)
              (list (make-piece 3 'blue) (make-piece 2 'orange)))

(check-expect (pieces-at (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)
              8)
              empty)
              
(define (pieces-at b i)
  (list-ref b i))

;; pieces-valid? : game -> bool
;; Test that the collection of pieces in the game (on the board or in an inventory) 
;; includes exactly the pieces it should (that is, two of each size and six of both colors),
;; and that each inventory contains only pieces of the right color.
(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) true)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'blue)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'orange)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 2 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) true)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 3 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (pieces-valid? (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 1 'blue) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'blue)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)




(define (pieces-valid? g)
  (local ( (define (num-of size player)  ;; this goes through the inventory of the indicated player
                                         ;; and sums the number of pieces that satisfy size. 
                                         (+ (foldl (λ (p acc) (if (and (symbol=? player (piece-player p))
                                                                                   (= size (piece-size p)))
                                                                              (add1 acc)
                                                                              acc)) 0 (if (symbol=? player 'blue)
                                                                                            (game-binv g)
                                                                                            (game-oinv g)))
                                         ;; this goes through the board and sums the number of pieces 
                                         ;; satisfy size and player. 
                                         (foldl (λ (lst acc1) 
                                               (+ acc1 (foldl (λ (p acc2) 
                                                                (if (and (symbol=? player (piece-player p))
                                                                                   (= size (piece-size p)))
                                                                              (add1 acc2)
                                                                              acc2)) 0 lst))) 0 (game-board g))))
           (define 2-of-each (= 2 (num-of 1 'blue)
                                  (num-of 2 'blue)
                                  (num-of 3 'blue)
                                  (num-of 1 'orange)
                                  (num-of 2 'orange)
                                  (num-of 3 'orange))))
                                  
     2-of-each))

;; #grader: Keep lines under 80 characters
                                      
           

;; squares-valid? : board -> bool
;; Test whether all squares on the board are in a legal state: 
;; that is, that there are zero to three pieces on each square, 
;; and that all gobbling is well-founded (every piece that gobbles 
;; another piece is strictly larger than what it gobbles). 
;; Note that (as stated above) the list of pieces at each square
;; is in outer-to-inner order.

(check-expect (squares-valid? 
               (game-board (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'blue) (make-piece 1 'orange)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)))) true)

(check-expect (squares-valid? 
               (game-board (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'blue) (make-piece 1 'orange)) 
                   empty 
                   (list (make-piece 3 'orange) (make-piece 2 'orange) (make-piece 1 'blue)) 
                   (list (make-piece 3 'orange) (make-piece 2 'blue) (make-piece 1 'orange) (make-piece 1 'orange))
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)))) false)

(check-expect (squares-valid? 
               (game-board (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)))) false)

(check-expect (squares-valid? 
               (game-board (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 1 'blue)
                   (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue))

             (list (list (make-piece 2 'orange) (make-piece 1 'blue) (make-piece 3 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty)))) false)


(define (squares-valid? b)
  (local ( (define (gobble lst)
             (if (or (empty? lst) (empty? (cdr lst)))
                 true
                 (if (> (piece-size (car lst)) (piece-size (cadr lst)))
                     (gobble (cdr lst))
                     false)))
           ;; if any of the squares is false for gobble then ill-founded will be true.
           ;; using ormap is more effcient than andmap, with which we need to check
           ;; every single square for that gobble is true. 
           (define ill-founded (ormap (λ (lst) (not (gobble lst))) b)) 
           )
   (not ill-founded)))


           
;; square-available? : piece square board -> bool
;; A square is available to a given piece if it is
;; either empty, or non-empty but able to be gobbled
;; by that piece.

(check-expect (square-available? (make-piece 3 'blue)
                                 2
                                 (list
                                  empty
                                  empty
                                  (list (make-piece 2 'blue) (make-piece 1 'orange))
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty)) true)

(check-expect (square-available? (make-piece 3 'blue)
                                 3
                                 (list
                                  empty
                                  empty
                                  (list (make-piece 2 'blue) (make-piece 1 'orange))
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty)) true)

(check-expect (square-available? (make-piece 3 'orange)
                                 2
                                 (list
                                  empty
                                  empty
                                  (list (make-piece 3 'orange) (make-piece 2 'blue) (make-piece 1 'orange))
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty
                                  empty)) false)

(define (square-available? p s b)
  (local ( (define square (pieces-at b s)) )
    (if (empty? square) 
        true
        (and (> (piece-size p) (piece-size (car square)))
             (< (length square) 3)))))
    

;; move-legal? : move game -> bool
;; Test whether a given move is legal in a given game state:
;; that the player to move actually possesses the piece in 
;; question (in the case of an intro), and that the destination 
;; square is available (per the previous function).

(check-expect (move-legal? (make-intro (make-piece 3 'blue) 2)
                                         
             (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) true)

(check-expect (move-legal? (make-intro (make-piece 3 'orange) 2)
                                         
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) true)

(check-expect (move-legal? (make-intro (make-piece 3 'orange) 2)
                                         
             (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (move-legal? (make-intro (make-piece 1 'blue) 4)
                                         
             (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (move-legal? (make-shift 2 0)
                                         
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (move-legal? (make-shift 2 2)
                                         
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty 
                   (list (make-piece 1 'blue))
                   empty))) false)

(check-expect (move-legal? (make-shift 1 2) 
                          (make-game 'orange
                                     (list (make-piece 1 'orange)
                                           (make-piece 2 'orange)
                                           (make-piece 3 'orange)
                                           (make-piece 3 'orange))
                                     (list (make-piece 2 'blue)
                                           (make-piece 2 'blue)
                                           (make-piece 3 'blue)
                                           (make-piece 3 'blue))
                                     (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                                           empty 
                                           (list (make-piece 2 'orange)) 
                                           empty
                                           empty 
                                           empty 
                                           empty 
                                           (list (make-piece 1 'blue))
                                           empty))) false)


(define (move-legal? m g)
   (match m
         [(intro p s) (and (if (equal? (game-next g) 'blue)
                                    (member p (game-binv g))
                                    (member p (game-oinv g)))
                                (square-available? p s (game-board g)))]
         [(shift src dst) (and
                           (not (empty? (pieces-at (game-board g) src)))
                           (equal? (piece-player (car (list-ref (game-board g) src))) (game-next g))
                           (square-available? (car (list-ref (game-board g) src)) dst (game-board g)))]))



;; victory? : player game -> bool 
;; Test whether the given player is victorious in the given game state.

(check-expect (victory? 'blue 
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   (list (make-piece 2 'blue))
                   (list (make-piece 2 'blue))
                   (list (make-piece 1 'blue))))) true) 

(check-expect (victory? 'blue 
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty
                   (list (make-piece 2 'blue))
                   (list (make-piece 1 'blue))))) false) 


(define (victory? p g)
  (local {  (define board (game-board g))
            (define v-squares '( (0 1 2) (3 4 5) (6 7 8) (0 3 6) 
                                 (1 4 7) (2 5 8) (0 4 8) (2 4 6) ) ) }
    (ormap (lambda (lst) (andmap (lambda (n)   (and (not (empty? (pieces-at board n)))
                                               (local { (define p2 (piece-player (car (pieces-at board n)))) }
                                               (equal? p p2)))) lst)) v-squares))) 

;; apply-move : move game -> game
;; Apply the given move to the game and return the game's subsequent state.
;; Note that an intro move has the effect of both adding a piece to the board 
;; and removing it from an inventory. If the proposed move is illegal, raise 
;; an error.
(check-expect(apply-move (make-intro (make-piece 3 'orange) 8)
             (make-game 'orange
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty
                   (list (make-piece 2 'blue))
                   (list (make-piece 1 'blue)))) )  
              
              (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 2 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 3 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   empty 
                   empty 
                   empty
                   (list (make-piece 2 'blue))
                   (list (make-piece 3 'orange) (make-piece 1 'blue))))) 



(check-expect(apply-move (make-shift 4 5)
             (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 1 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   (list (make-piece 3 'blue) (make-piece 2 'orange))
                   empty 
                   empty
                   (list (make-piece 2 'blue))
                   (list (make-piece 1 'orange)))) )  
              
             (make-game 'blue
             (list (make-piece 1 'orange)
                   (make-piece 3 'orange))
             (list (make-piece 3 'blue)
                   (make-piece 2 'blue)
                   (make-piece 3 'blue)
                   (make-piece 1 'blue))
             (list (list (make-piece 3 'orange) (make-piece 1 'blue)) 
                   empty 
                   (list (make-piece 2 'orange)) 
                   empty
                   (list (make-piece 2 'orange)) 
                   empty 
                   empty
                   (list (make-piece 2 'blue))
                   (list (make-piece 1 'orange))))) 

(check-expect (apply-move 
               (make-intro (make-piece 3 'blue) 4)
               (make-game 'blue
                          (list (make-piece 1 'orange)
                                (make-piece 1 'orange)
                                (make-piece 2 'orange)
                                (make-piece 2 'orange)
                                (make-piece 3 'orange)
                                (make-piece 3 'orange))
                          (list (make-piece 1 'blue)
                                (make-piece 1 'blue)
                                (make-piece 2 'blue)
                                (make-piece 2 'blue)
                                (make-piece 3 'blue)
                                (make-piece 3 'blue))
                          (list empty empty empty empty
                                empty empty empty empty
                                empty)))
              (make-game 'orange
                          (list (make-piece 1 'orange)
                                (make-piece 1 'orange)
                                (make-piece 2 'orange)
                                (make-piece 2 'orange)
                                (make-piece 3 'orange)
                                (make-piece 3 'orange))
                          (list (make-piece 1 'blue)
                                (make-piece 1 'blue)
                                (make-piece 2 'blue)
                                (make-piece 2 'blue)
                                (make-piece 3 'blue))
                          (list empty empty empty empty
                                (list (make-piece 3 'blue)) empty empty empty
                                empty)))

(check-expect (apply-move 
               (make-shift 3 4) 
               (make-game 'orange
                         (list (make-piece 2 'orange)
                               (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list (list (make-piece 1 'blue)) 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 3 'orange) (make-piece 1 'blue))
                               empty
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty)))
              (make-game 'orange
                         (list (make-piece 3 'orange)  
                               (make-piece 2 'orange)
                               (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list (list (make-piece 1 'blue)) 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 1 'blue))
                               empty
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty)))

(check-expect (apply-move 
               (make-shift 0 4) 
               (make-game 'blue
                         (list (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list (list (make-piece 1 'blue)) 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 3 'orange) (make-piece 2 'orange) (make-piece 1 'blue))
                               empty
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty)))
              (make-game 'orange
                         (list (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list empty 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 3 'orange) (make-piece 2 'orange) (make-piece 1 'blue))
                               (list (make-piece 1 'blue))
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty)))

(check-error (apply-move 
              (make-shift 1 4) 
              (make-game 'blue
                         (list (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list (list (make-piece 1 'blue)) 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 3 'orange) (make-piece 2 'orange) (make-piece 1 'blue))
                               empty
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty))))

(check-error (apply-move 
              (make-shift 0 1) 
              (make-game 'blue
                         (list (make-piece 2 'orange)
                               (make-piece 3 'orange))
                         (list (make-piece 2 'blue)
                               (make-piece 3 'blue)
                               (make-piece 3 'blue))
                         (list (list (make-piece 1 'blue)) 
                               (list (make-piece 1 'orange)) 
                               empty
                               (list (make-piece 3 'orange) (make-piece 2 'orange) (make-piece 1 'blue))
                               empty
                               empty
                               (list (make-piece 2 'blue))
                               (list (make-piece 1 'orange))
                               empty))))
                         

(define (apply-move m g)
  (if (move-legal? m g)
  (match m
    [(intro p s) (local ( (define current (game-next g))
                          (define oinv (if (equal? current 'orange)
                                           (remove p (game-oinv g))
                                           (game-oinv g)))
                          (define binv (if (equal? current 'blue)
                                           (remove p (game-binv g))
                                           (game-binv g))) )
                         
                         (make-game 
                          (other (game-next g)) 
                          oinv 
                          binv
                          (proc-from-board cons (intro-piece m) (intro-square m) (game-board g))))]

    [(shift src dst) (local ((define shifty-piece (car (list-ref (game-board g) src)))
                             (define board-during-shift (proc-from-board remove shifty-piece src (game-board g)))
                             (define game-during-shift (if (equal? (game-next g) 'blue)
                                                           (make-game (game-next g) (game-oinv g)  (cons shifty-piece (game-binv g)) board-during-shift)
                                                           (make-game (game-next g) (cons shifty-piece (game-oinv g))  (game-binv g) board-during-shift)))) 
                        ;; tests if the shift results in a reveal-victory for the other player
                        ;; returns a frozen game state with current player still as next if true
                        ;; returns a new game state with piece shifted otherwise. 
                       (if (victory? (other (game-next g)) game-during-shift)
                           game-during-shift
                           (apply-move (make-intro shifty-piece dst) game-during-shift)))])
  (error "illegal move")))

;; other : player -> player
;; returns the other player
(check-expect (other 'blue) 'orange)
(check-expect (other 'orange) 'blue)
(define (other p)
  (match p
    ['blue 'orange]
    ['orange 'blue]))


;; proc-from-board : (piece (listof pieces) -> (listof pieces)) piece square board -> board
;; does a procedure with a piece at
;; the specified piece to the specified position on a board
(check-expect (proc-from-board cons (make-piece 3 'orange) 3 (list (list (make-piece 2 'blue))
                   (list (make-piece 3 'blue))
                   (list (make-piece 1 'orange))
                   empty
                   (list (make-piece 2 'orange) (make-piece 1 'orange))
                   empty
                   empty
                   empty
                   empty))
              (list (list (make-piece 2 'blue))
                   (list (make-piece 3 'blue))
                   (list (make-piece 1 'orange))
                   (list (make-piece 3' orange))
                   (list (make-piece 2 'orange) (make-piece 1 'orange))
                   empty
                   empty
                   empty
                   empty))

(check-expect (proc-from-board cons (make-piece 3 'orange) 0 (list (list (make-piece 2 'blue))
                   (list (make-piece 3 'blue))
                   (list (make-piece 1 'orange))
                   empty
                   (list (make-piece 2 'orange) (make-piece 1 'orange))
                   empty
                   empty
                   empty
                   empty))
              (list (list (make-piece 3 'orange) (make-piece 2 'blue))
                   (list (make-piece 3 'blue))
                   (list (make-piece 1 'orange))
                   empty
                   (list (make-piece 2 'orange) (make-piece 1 'orange))
                   empty
                   empty
                   empty
                   empty))

(check-expect (proc-from-board remove (make-piece 3 'orange) 0 
                               (list (list (make-piece 3 'orange) (make-piece 2 'blue))
                                     (list (make-piece 3 'blue))
                                     (list (make-piece 1 'orange))
                                     empty
                                     (list (make-piece 2 'orange) (make-piece 1 'orange))
                                     empty
                                     empty
                                     empty
                                     empty))
              (list (list (make-piece 2 'blue))
                    (list (make-piece 3 'blue))
                    (list (make-piece 1 'orange))
                    empty
                    (list (make-piece 2 'orange) (make-piece 1 'orange))
                    empty
                    empty
                    empty
                    empty))


(define (proc-from-board proc p s b)
   (local { (define (add n)
            (if (> n 8)
                empty
                (if (= n s) 
                    (cons (proc p (pieces-at b s)) (add (add1 n)))
                    (cons (pieces-at b n) (add (add1 n)))))) }
    (add 0)))

;;-------------------------------------------------------------
;;-------------------------Visualization-----------------------
;;-------------------------------------------------------------

;; board-image : board -> image
;; draws a view of the board from top view
;; eyeball tested

(define (board-image b)
  (local { (define lst-of-piece-boxes (map opaque-box (map (λ(lst) (if (empty? lst)
                                                                  empty
                                                                  (car lst))) b))) }
    (stack-by-3 lst-of-piece-boxes)))


;; opaque-box : piece -> image
;; draws a box with opaque circles
;; eyeball tested
(define (opaque-box p)
  (if (empty? p)
      (square 62 "outline" "black")
      (local { (define size (cond [(= (piece-size p) 1) 10]
                                  [(= (piece-size p) 2) 20]
                                  [(= (piece-size p) 3) 30])) }
        (underlay (square 62 "outline" "black")
                  (circle size "solid" (symbol->string (piece-player p)))))))

 
;; stack-by-3 : (listof image) -> image
;; stacks a list of boxes together to construct the board
;; eyeball tested
(define (stack-by-3 loi)
  (if (= 3 (length loi))
     (beside (car loi) (cadr loi) (caddr loi))
     (above (beside  (car loi) (cadr loi) (caddr loi))
                 (stack-by-3 (cdddr loi))))) 
 

;; xray-board-image : board -> image
;; draws all pieces on a board in x ray style 
;; eyeball tested
(define (xray-board-image b)
  (local { (define lst-of-piece-boxes (map xray-box (map (λ(lst) (if (empty? lst)
                                                                  empty
                                                                  lst)) b))) }
    (stack-by-3 lst-of-piece-boxes)))


;; xray-box : (listof pieces) -> image
;; draws a box with outlines of circles inside such that
;; all pieces are seen
;; eyeball tested
(define (xray-box lop)
  (if (empty? lop)
      (square 62 "outline" "black")
      (underlay (square 62 "outline" "black")
                (concentric-circles lop))))

;; concentric-circles: (listof pieces) -> image
;; stacks outlines of circles on top of one another
;; eyeball tested
(define (concentric-circles lop)
  (if (empty? lop) 
      empty-image
  (local { (define size (cond   [(= (piece-size (car lop)) 1) 10]
                                [(= (piece-size (car lop)) 2) 20]
                                [(= (piece-size (car lop)) 3) 30]))
           (define c (cond [(equal? (piece-player (car lop)) 'blue) "blue"]
                           [else "orange"]))}
    (underlay (circle size "outline" c)
              (concentric-circles (cdr lop))))))

;; game-image : bool game -> image
;; visualize the entire game state, boolean indicates whether xray or opaque
;; eyeball tested
(define (game-image xray g)
  (above (graphic-turn g)
         (if xray
             (xray-board-image (game-board g))
             (board-image (game-board g)))
         (square 20 "solid" "white")
         (graphic-inv g)))

;; graphic-turn : game -> image
;; returns a graphic representation of whose turn is next. 
;; eyeball tested
(define (graphic-turn g)
  (local { (define next (cond [(equal? (game-next g) 'blue) "Blue"]
                           [else "Orange"])) }
    (text (string-append next " is next") 20 next)))

;; graphic-inv : game -> image
;; returns a graphic representation of both player's inventories
;; eyeball tested
(define (graphic-inv g)
  (local { (define oinv (foldr (λ (p acc) (beside acc (cond [(= (piece-size p) 1) (circle 10 "solid" "orange")]
                                                            [(= (piece-size p) 2) (circle 20 "solid" "orange")]
                                                            [(= (piece-size p) 3) (circle 30 "solid" "orange")]))) empty-image (game-oinv g)))
           (define binv (foldr (λ (p acc) (beside acc (cond [(= (piece-size p) 1) (circle 10 "solid" "blue")]
                                                            [(= (piece-size p) 2) (circle 20 "solid" "blue")]
                                                            [(= (piece-size p) 3) (circle 30 "solid" "blue")]))) empty-image (game-binv g))) }
    (overlay/align/offset "left" "top"
                          oinv
                          0
                          100
                          binv)))


"---------------- grader's test ----------------"

(require racket/match)

;; some predefined objects for convenience

(define o1 (make-piece 1 'orange))
(define o2 (make-piece 2 'orange))
(define o3 (make-piece 3 'orange))
(define b1 (make-piece 1 'blue))
(define b2 (make-piece 2 'blue))
(define b3 (make-piece 3 'blue))

(define g-orange-full-inventory (list o1 o1 o2 o2 o3 o3))
(define g-blue-full-inventory (list b1 b1 b2 b2 b3 b3))
(define g-orange-inventory1 (list o1 o2 o2 o3 o3))
(define g-blue-inventory1 (list b1 b1 b2 b3 b3))

(define g-intro1 (make-intro o1 2))
(define g-board1 (list empty empty (list o1) empty empty empty empty empty empty))
(define g-intro2 (make-intro b2 2))
(define g-board2 (list empty empty (list b2 o1) empty empty empty empty empty empty))
(define g-intro3 (make-intro o3 3))
(define g-board3 (list empty empty (list b2 o1) (list o3) empty empty empty empty empty))
(define g-shift1 (make-shift 2 4))
(define g-board4 (list empty empty (list o1) (list o3) (list b2) empty empty empty empty))
(define g-intro4 (make-intro o3 4))
(define g-board5 (list empty empty (list o1) (list o3) (list o3 b2) empty empty empty empty))
(define g-intro5 (make-intro b2 0))
(define g-board6 (list (list b2) empty (list o1) (list o3) (list o3 b2) empty empty empty empty))
(define g-intro6 (make-intro o1 5))
(define g-board7 (list (list b2) empty (list o1) (list o3) (list o3 b2) (list o1) empty empty empty))

(define g-empty-board (make-list 9 empty))

(define g-bluegame0 (make-game 'blue g-orange-full-inventory g-blue-full-inventory g-empty-board))
(define g-orangegame0 (make-game 'orange g-orange-full-inventory g-blue-full-inventory g-empty-board))
(define g-bluegame1 (make-game 'blue g-orange-inventory1 g-blue-full-inventory g-board1))
(define g-orangegame1 (make-game 'orange g-orange-inventory1 g-blue-inventory1 g-board2))
(define g-bluegame2 (make-game 'blue (list o1 o2 o2 o3) (list b1 b1 b2 b3 b3) g-board3))
(define g-orangegame2 (make-game 'orange (list o1 o2 o2 o3) (list b1 b1 b2 b3 b3) g-board4))
(define g-bluegame3 (make-game 'blue (list o1 o2 o2) (list b1 b1 b2 b3 b3) g-board5))
(define g-orangegame3 (make-game 'orange (list o1 o2 o2) (list b1 b3 b3) g-board6))
(define g-bluegame4 (make-game 'blue (list o2 o2) (list b1 b3 b3) g-board7))

(define (g-player=? p1 p2)
  (symbol=? p1 p2))

(define (g-contain? i1 i2)
  (cond [(empty? i2) true]
        [else (and (member (first i2) i1) (g-contain? i2 (rest i2)))]))

;; test if two inventories contains the same pieces. order doesn't matter.
(define (g-inventory=? i1 i2)
  (and (g-contain? i1 i2) (g-contain? i2 i1)))

;; test if two games are the same. pices' order in inventories doesn't matter.
(define (g-game=? g1 g2)
  (match* (g1 g2)
    [((game n1 oinv1 binv1 bd1) (game n2 oinv2 binv2 bd2)) 
     (and (symbol=? n1 n2) (g-inventory=? oinv1 oinv2)
          (g-inventory=? binv1 binv2) 
          (equal? bd1 bd2))]))


;; new-game
(check-expect (g-game=? (new-game 'blue) g-bluegame0) true)
(check-expect (g-game=? (new-game 'orange) g-orangegame0) true)

;; pieces-at
(check-expect (pieces-at g-board1 0) empty)
(check-expect (pieces-at g-board1 2) (list o1))
(check-expect (pieces-at g-board2 2) (list b2 o1))

;; pieces-valid?
(check-expect (pieces-valid? g-orangegame1) true)
(check-expect (pieces-valid? g-bluegame1) true)
(check-expect (pieces-valid? 
               (make-game 'orange g-orange-full-inventory g-blue-full-inventory g-board1)) 
              false)
(check-expect (pieces-valid? (make-game 'orange g-orange-inventory1 g-blue-inventory1 g-empty-board))
              false)
(check-expect (pieces-valid? (make-game 'orange g-orange-full-inventory g-orange-full-inventory g-empty-board))
              false)

;; squares-valid?
(check-expect (squares-valid? g-board1) true)
(check-expect (squares-valid? g-board2) true)
(check-expect (squares-valid? (append (list (list o1 o2)) (make-list 8 empty))) false)
(check-expect (squares-valid? (append (make-list 8 empty) (list (list o3 b1 b1)))) false)

;; square-available?
(check-expect (square-available? o3 0 g-board1) true)
(check-expect (square-available? o3 2 g-board2) true)
(check-expect (square-available? o2 2 g-board2) false)
(check-expect (square-available? b1 3 g-board3) false)

;; move-legal?
(check-expect (move-legal? g-intro1 g-bluegame0) false)
(check-expect (move-legal? g-intro1 g-orangegame0) true)
(check-expect (move-legal? g-intro1 g-bluegame1) false)
(check-expect (move-legal? g-shift1 g-orangegame1) false)
(check-expect (move-legal? g-shift1 g-bluegame2) true)
(check-expect (move-legal? (make-intro o3 0) g-orangegame3) false)

;; victory:
(check-expect (victory? 'blue g-bluegame4) false)
(check-expect (victory? 'orange g-bluegame4) true)
(check-expect (victory? 'orange g-bluegame3) false)

;; apply-move
(check-expect (g-game=? (apply-move g-intro1 g-orangegame0) g-bluegame1) true)
(check-expect (g-game=? (apply-move g-intro2 g-bluegame1) g-orangegame1) true)
(check-expect (g-game=? (apply-move g-intro3 g-orangegame1) g-bluegame2) true)
(check-expect (g-game=? (apply-move g-shift1 g-bluegame2) g-orangegame2) true)
(check-expect (g-game=? (apply-move g-intro3 g-orangegame0) g-bluegame1) false)

"board-image:"
(board-image g-board1)
(board-image g-board6)

"xray-board-image:"
(xray-board-image g-board6)

"game-image"
(game-image true g-orangegame2)
(game-image true g-bluegame3)
(game-image false g-bluegame3)

;; === evaluation ===

;; === correctness ===

;; new-game            6/  6
;; pieces-at           4/  4
;; pieces-valid?       10/ 10
;; squares-valid?      10/ 10
;; square-available?   10/ 10
;; move-legal?         10/ 10
;; victory?            10/ 10
;; apply-move          10/ 10

;; board-image         10/ 10
;; xray-board-image    10/ 10
;; game-image          10/ 10

;; _subtotal_          100/100

;; === style + svn ===

;; code layout                       5/ 6 (One or two long lines)
;; identifiers are well named        6/ 6
;; program decomposition (helpers)   8/ 8
;; contracts                         6/ 6
;; well-written purposes             6/ 6
;; adequate tests                    6/ 6
;; svn usage                         2/ 2
;; _subtotal_                       39/40

;; _total-score_ 139/140

;; grader: Luke Peeler