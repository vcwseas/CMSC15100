;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 8-Queen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/match)
;; σ is a type variable, a mnemonic for "state."

;; search : σ (σ -> (or (listof σ) 'goal)) -> (or σ false)
;; generic backtrack search starting from the start state.  The next
;; function either returns 'goal, when its argument is a goal state,
;; '() when its argument is a dead end, or else a list of successor
;; states.
(define (search start next)
  (local
    {;; look : σ -> (or σ false)
     ;; starting from given state, return a goal state or false
     (define (look s)
       (match (next s)
         ['goal s]
         [next-states
          (local
            {;; lp : (listof σ) -> (or σ false)
             ;; look for a goal from list of states;
             ;; return first goal encountered, or false 
             (define (lp ss)
               (cond
                [(empty? ss) false]
                [else 
                 (match (look (first ss))
                   [#f (lp (rest ss))]
                   [g g])]))}
            (lp next-states))]))}
    (look start))) 


(define (search-acc state next)
  (local { (define (look s acc-set)
             (match (next s)
               ['goal s]
               [next-states
                (look s (cons (local { (define (lp ss acc)
                           (cond
                             [(empty? ss)  false]
                             [else
                              (match (look (first ss) acc)
                                [#f  (lp (rest ss) acc)]
                                [g (lp (rest ss) (cons g acc))])])) }
                  (lp next-states '())) acc-set))])) }
    (look state '())))
             

;; a 8-q-state is a (listof posn)
;; where posn represent the position of a queen
;; on an 8 x 8 board. 

(define soln1 (list (make-posn 0 3) (make-posn 1 6)
                        (make-posn 2 2) (make-posn 3 7)
                        (make-posn 4 1) (make-posn 5 4)
                        (make-posn 6 0) (make-posn 7 5)))
(define test-1 (list (make-posn 0 3) (make-posn 1 6)
                        (make-posn 2 2) (make-posn 3 7)
                        (make-posn 4 1) (make-posn 5 4)))
                                        

;; threaten? : 8-q-state -> bool
;; if one queen threatens another in the current
;; configuration then returns true.
(define (threaten? s)
  (local { (define (threat? p1 p2)
             (or (= (posn-x p1) (posn-x p2))
                 (= (posn-y p1) (posn-y p2))
                 (= (abs (- (posn-x p1) (posn-x p2)))
                     (abs (- (posn-y p1) (posn-y p2)))))) 
           (define (threat?-state p1) 
             (ormap (λ (p2) (if (equal? p1 p2)
                              #f
                           (threat? p1 p2))) s)) }
    (ormap (λ (p) (threat?-state p)) s)))
    


;; q-next: 8-q-state -> (or (listof 8-q-state) 'goal))
;; returns a list of possible next states or 'goal 
(define (q-next s)
  (cond
    [(and (= (length s) 8) (not (threaten? s))) 'goal]
    [(= (length s) 8) '()]
    [else (local { (define row (length s))
                   (define next (foldl (λ (col acc) (local 
                                                      { (define state-to-try (cons (make-posn row col) s)) }
                                                      (if (threaten? state-to-try)
                                                          acc
                                                          (cons state-to-try acc)))) '() (build-list 8 identity))) }
            next)]))
              