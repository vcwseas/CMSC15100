;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname proj1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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