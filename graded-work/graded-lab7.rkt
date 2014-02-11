;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 John Reppy CMSC 15100 Aug 13
(require 2htdp/image)
(require racket/match)
;;;;;;;;;;;;;;;;;;;; PLAYER, MOVE, AND GAME DATA DEFINITIONS ;;;;;;;;;;;;;;;;;;;;

;; a (pair X Y) is a (make-pair x y), where 
;; x is an X and y is a Y.
(define-struct pair (fst snd))

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

;; a shft is a (make-shft src dst) where
;; - src and dst are both squares
;; NOTE: the name of this data structure has changed
;;       to avoid conflict with the name "shift" in universe
(define-struct shft (src dst))

;; a move is either
;; - an intro, or
;; - a shft

;; an inventory is a (listof piece) ordered by increasing size

;; a board is a (listof (listof piece)) representing a rectangular
;; grid of squares.  The playing board is a 3x3 grid, while the
;; inventory boards are 3x2 grids. Each square is represented
;; by a list of pieces, where the pieces are ordered from outermost
;; (i.e., biggest) to innermost (smallest).  An square with no pieces
;; is represented by the empty list.  The order of the squares in
;; this list is row-major order; for example, the playing board
;; squares are ordered as follows:
;;
;;    0 1 2
;;    3 4 5
;;    6 7 8

;; a game is a (make-game next inv board) where
;; - next is a player, 
;; - inv is a pair (make-pair oinv binv), where
;;   - oinv ("orange inventory") is an inventory,
;;   - binv ("blue inventory") is an inventory, and
;; - board is a board (per definition above)
;; NOTE: the inventories are stored in a pair structure now;
;;       this differs from the project 1 design
(define-struct game (next oinv binv board))



;;---------------------------------------------------
;;-----------------------Lab7------------------------
;;---------------------------------------------------

;; a board-set is a (listof board)
;; where each board in the same list is
;; in the same equivalence class. Equivalence
;; class refers to a set of at most eight boards
;; where the at most four of the 
;; boards are 90 degree ratations of one another
;; or where the boards are 90 degree rotations of the vertical
;; flip of the original board.
;; board-set does not contain duplicate values. 

;; board-set-insert: board board-set -> board-set
;; inserts a board into the board-set
;; grader: You need to make sure that there are no duplicates.
(define (board-set-insert b bs)
  (cons b bs))


;; board-set-member? : board board-set -> bool
;; checkes if a board is in a board-set
(define (board-set-member? b bs)
  (member b bs))


;; board-invert : board -> board
;; inverts all colors of pieces in a board
(define (board-invert b)
  (local { (define (invert-p p) (if (equal? (piece-player p) 'blue)
                                    (make-piece (piece-size p) 'orange)
                                    (make-piece (piece-size p) 'blue)))
           (define (invert-lop lop) (map invert-p lop))
           (define (invert-board b) (map invert-lop b)) }
    (invert-board b)))


;; board-equiv-class : board -> board-set
;; builds the equivalent class for a board position
;; eyeball tested with the board configurations on the website
(define (board-equiv-class b)
  (local { (define fliped-b (map (λ (n) (list-ref b n)) '(2 1 0 5 4 3 8 7 6)))
           ;; loop4: board -> (listof boards)
           ;; rotates a board 90 degree and returns a list of the results
           (define (loop4 b)
            (local { (define (l counter lob)
                       (if (< counter 4)
                                (l (add1 counter) 
                                    (board-set-insert (map (λ (n) (if (empty? lob)
                                                          (list-ref b n)
                                                          (list-ref (car lob) n))) '(2 5 8 1 4 7 0 3 6)) lob))
                                lob)) }
             (l 0 '() )))
           ;; rm-same-board: (listof board) (listof board) -> (listof board)
           ;; removes duplicate boards from the board-set
           ;; grader: Instead of removing you should try to contstruct a set using board-set-insert.
           (define (rm-same-board r-lob n-lob)
             (cond
               [(empty? r-lob) n-lob]
               [else (local { (define (rm b lob)
                                (if (empty? lob)
                                    b
                                    (if (equal? b (car lob))
                                        '()
                                        (rm b (cdr lob))))) }
                       
                       (rm-same-board (cdr r-lob) (local { (define unique-board (rm (car r-lob) (cdr r-lob))) }
                                                    (if (empty? unique-board)
                                                        n-lob
                                                        (cons unique-board n-lob)))))]))}
                                    
                
                
                
           (reverse (rm-same-board (append  (loop4 b) (loop4 fliped-b)) '() ))))




;; board-equiv? : board board -> bool
;; returns whether or not two boards are in the same equivalence class
(define (board-equiv? b1 b2)
  (local { (define equiv-class (board-equiv-class b1)) }
    (board-set-member? b2 equiv-class)))



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
         (if xray
             (xray-board-image (game-board g))
             (board-image (game-board g))))


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

;;-------------------------------------------------------------
;;-------------------------Tests-------------------------------
;;-------------------------------------------------------------


;; test pieces
(define o1 (make-piece 1 'orange))
(define o2 (make-piece 2 'orange))
(define o3 (make-piece 3 'orange))
(define b1 (make-piece 1 'blue))
(define b2 (make-piece 2 'blue))
(define b3 (make-piece 3 'blue))

;; test game
(define test (make-game 'blue '() '() (list (list (make-piece 3 'blue))
                               (list (make-piece 1 'orange))
                               empty empty empty empty empty empty empty)))


(check-expect (board-equiv? (list
                             (list b3)
                             (list o1)
                             empty empty empty
                             empty empty empty
                             empty)
                            (list
                             empty empty empty
                             empty empty empty 
                             empty (list o1) (list b3)))
              true)

(check-expect (board-equiv? (list
                             (list b3)
                             (list o1)
                             empty empty empty
                             empty empty empty
                             empty)
                            (list
                             (list b3) empty empty
                             (list o1) empty empty
                             empty empty empty))
              #t)

(check-expect (board-equiv? (list
                             (list b1) empty empty
                             empty empty empty
                             empty empty empty)
                            (list
                             (list b1) empty empty 
                             empty empty empty
                             empty empty empty))
              #t)

(check-expect (board-equiv? site-last-board
                            (list
                             empty
                             (list b3)
                             (list o1)
                             (list o2 o1)
                             (list b1)
                             (list b2)
                             (list o3 o2 b1)
                             (list o3 b2)
                             (list b3)))
              #t)

(check-expect (length (board-equiv-class (list 
                                          (list o1) empty empty
                                          empty empty empty
                                          empty empty empty)))
              4)
  
(check-expect (length (board-equiv-class (list 
                                          empty empty empty
                                          empty (list o3 o2 o1) empty
                                          empty empty empty)))
              1)                                        
                             

(define site-last-board (list
                         (list o3 o2 b1)
                         (list o3 b2)
                         (list b3)
                         (list o2 o1)
                         (list b1)
                         (list b2)
                         empty
                         (list b3)
                         (list o1)))

"These are the boards at the end of lab7 from the site"

(map (λ(g)(game-image true g)) 
     (map (λ (b) (make-game 'blue '() '() b)) 
          (board-equiv-class site-last-board)))

"This is the board-set of a board with only pieces in the center"
(map (λ(g)(game-image true g)) 
     (map (λ (b) (make-game 'blue '() '() b)) 
          (board-equiv-class (list empty empty empty
                                   empty (list o3 o2 o1) empty
                                   empty empty empty))))
"This is the board-set of a board with only pieces in one corner"
(map (λ(g)(game-image true g)) 
     (map (λ (b) (make-game 'blue '() '() b)) 
          (board-equiv-class (list (list b3 o2 b1) empty empty
                                   empty  empty empty
                                   empty empty empty))))
"This is the board-set of a board with only pieces in two opposing corners"
(map (λ(g)(game-image true g)) 
     (map (λ (b) (make-game 'blue '() '() b)) 
          (board-equiv-class (list (list o3 b2 o1) empty empty
                                   empty empty empty
                                   empty empty (list b3 o2 b1)))))

;; === evaluation ===

;; === correctness ===

;; board-set-member?             7/ 7
;; board-set-insert              2/ 7
;; board-invert                  8/ 8
;; board-equiv-class            10/10
;; board-equiv?                  6/ 6

;; _subtotal_                   33/38

;; === style + svn ===

;; code layout                       6/ 6
;; identifiers are well named        6/ 6
;; program decomposition (helpers)   5/ 6
;; contracts                         6/ 6
;; well-written purposes             6/ 6
;; adequate tests                    6/ 6
;; svn usage                         2/ 2
;; _subtotal_                       37/38

;; _total-score_ 70/76

;; grader: Tristan Rasmussen