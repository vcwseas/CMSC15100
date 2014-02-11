;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 CMSC 15100 AUT 13 John Reppy
(require racket/match)
;; -------------------------------------------------
;; -------------------Problem3----------------------
;; -------------------------------------------------

;; vec-max: (vec num) -> num
;; returns the max value of a vector of numbers
(define (vec-max v)
  (local { (define length (vector-length v))
           (define (max i m)
             (if (< i length)
                 (if (> (vector-ref v i) m)
                     (max (add1 i) (vector-ref v i))
                     (max (add1 i) m))
                 m)) }
    (if (= length 0)
        (error "vector cannot be empty")
        (max 0 (vector-ref v 0)))))

(check-expect (vec-max (vector 1 2 3 4 5 6)) 6)
(check-expect (vec-max (vector 0 0 0 0 0 0)) 0)
(check-error (vec-max (vector)))

;; vec-reduce : (α α -> α) α (vec α) -> α
;; reduce the vector given a commutative operator and an identity
(define (vec-reduce f z v) 
  (local { (define length (vector-length v))
           (define (fold i acc)
             (cond
               [(< i length) (fold (add1 i) (f (vector-ref v i) acc))]
               [else acc])) }
    (if (= length 0)
        z
        (fold 0 z))))

(check-expect (vec-reduce + 0 (vector 1 2 3 4 5)) 15)
(check-expect (vec-reduce * 1 (vector 1 2 3 4 5)) 120)
(check-expect (vec-reduce - 0 (vector)) 0)
(check-expect (vec-reduce min 1 (vector 1 2 3 4 5 6 0)) 0)
(check-expect (vec-reduce max 1 (vector 1 13 4 5 6 0 1 2 3 1000)) (vec-max (vector 1 13 4 5 6 0 1 2 3 1000)))
(check-expect (vec-reduce (λ(n acc) (append acc (list n)))  '() (vector 1 2 3 4 5)) '(1 2 3 4 5))

;; vec-indices : (α -> bool) (vec α) -> (listof num)
;; return the list of indices of values for which the test is true, 
;; in ascending order
(define (vec-indices pred v)
  (local { (define length (vector-length v))
           (define (filter i)
             (cond
               [(< i length) (if (pred (vector-ref v i))
                                 (cons i (filter (add1 i)))
                                 (filter (add1 i)))]
               [else '()])) }
    (filter 0))) 

(check-expect (vec-indices positive? (vector -1 -2 -3 -4 1 2 3 4)) '(4 5 6 7))
(check-expect (vec-indices procedure? (vector (λ (x) (* x x))
                                              (λ (f g) (λ (x) (f (g x))))
                                              (λ (f n) (λ (x) (f n x)))
                                              "Haskell Curry"))
              '(0 1 2))
(check-expect (vec-indices negative? (vector 1 2 3 4 5 6 7 8 9 1000000)) '())

;; -------------------------------------------------
;; -------------------Problem2----------------------
;; -------------------------------------------------
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

;; cp-state is a (vectorof bool) where
;; indicies 0...15 represent a 4 x 4 2D Array
;; #t indicates coin and #f indicates no coin.

;; cp-next: cp-state -> (or (listof cp-state) 'goal)
;; returns the reachable states from a given state or
;; 'goal is current state is the goal. 
(define (cp-next s)
  (cond
    [(cp-goal? s) 'goal]
    [else (if (cp-6-removed? s)
              '()
              (local { (define (rm-coin i acc)
                         (cond
                           [(>= i 16) acc]
                           [else (rm-coin (add1 i) (local { (define new-state (build-vector 16 (λ (n) (if (= n i)
                                                                                     #f
                                                                                     (vector-ref s n))))) }
                                                     (if (equal? s new-state)
                                                         acc
                                                         (cons new-state acc))))])) }
                (reverse (rm-coin 0 '()))))]))
(check-expect (cp-next (vector false false true true false true false true true false false true true true true true)) 'goal)
(check-expect (cp-next (vector false false false false false false #t #t #t #t #t #t #t #t #t #t)) '())
(check-expect (member  (vector false false false false false false #t #t #t #t #t #t #t #t #t #t)
                       (cp-next (vector true false false false false false #t #t #t #t #t #t #t #t #t #t))) true)


;; cp-6-removed? : cp-state -> bool
;; returns #t if all 6 coins have been removed
;; #f otherwise
(define (cp-6-removed? s)
  (= 6 (vec-reduce (λ (bool n) (if bool n (add1 n))) 0 s)))

(check-expect (cp-6-removed? (vector true true true true
                                  false false true true
                                  false false true true
                                  false false true true)) true)

(check-expect (cp-6-removed? (vector false false false false
                                   true true true true
                                   false false false false
                                   true true true true)) false)
     
;; cp-goal? : cp-state -> bool
;; returns #t if state is the goal
;; #f otherwise
(define (cp-goal? s)
  (local { (define row-list (list (list 0 1 2 3)
                                  (list 4 5 6 7)
                                  (list 8 9 10 11)
                                  (list 12 13 14 15)))
           (define col-list (list (list 0 4 8 12)
                                  (list 1 5 9 13)
                                  (list 2 6 10 14)
                                  (list 3 7 11 15)))
           (define (count-t lon)
             (cond
               [(empty? lon) 0]
               [else (if (vector-ref s (car lon))
                         (add1 (count-t (cdr lon)))
                         (count-t (cdr lon)))]))
           
           (define counted-rows (map count-t row-list))
           (define counted-cols (map count-t col-list)) }
    
    (and (cp-6-removed? s) (andmap even? counted-rows) (andmap even? counted-cols))))
                                                
(check-expect (cp-goal? (vector true false true false 
                                false true true false 
                                true true true true 
                                false false true true)) true)
(check-expect (cp-goal? (vector true false true false
                                true false true false
                                true false true false
                                true true true true)) false)



;; an ms-state is a (make-ms-state placed to-place) where
;; - placed is a (listof (or num false)) of length 9, and
;; - to-place is a (listof num) of length 0 to 9
(define-struct ms-state (placed to-place))

;; ms-next : ms-state -> (or (listof ms-state) 'goal)
;; returns a list of reachable states from given state
;; or 'goal is goal has been achieved. 
(define (ms-next s)
  (cond
    [(ms-goal? s) 'goal]
    [else (if (empty? (ms-state-to-place s)) 
              '()             
                  (build-state s))]))

(check-expect (ms-next (make-ms-state (list 2 7 6 9 5 1 4 3 8) '())) 'goal)
(check-expect (member (make-ms-state (list 1 #f #f #f 5 #f #f #f #f) (list 2 3 4 6 7 8 9)) (ms-next almoststate)) false)
(check-expect (member (make-ms-state (list 2 #f #f #f 5 #f #f #f #f) (list 1 3 4 6 7 8 9)) (ms-next almoststate)) true)

;; build-state: ms-state -> (listof ms-state)
;; returns a list of reachable states that could be solutions
;; rejects preemptively any state that has 2 4 6 8 in anywhere
;; but the corners, and any state hat has 5 in anywhere but the
;; center. 
(define (build-state s)
  (local { (define (bs n i acc)
             (local { (define new-to-place (remove n (ms-state-to-place s)))
                      (define placed (ms-state-placed s)) }
             (cond
               [(< i 9) (if (and (false? (list-ref placed i)) (if (= i 4) (= n 5) #t) (if (or (= i 0) (= i 2) (= i 6) (= i 8))
                                                                  (ormap (λ (m) (= n m)) '(2 4 6 8))
                                                                  #t))
                            (local { (define new-placed (make-ms-state (build-list 9 (λ (j) (if (= i j)
                                                                    n
                                                                    (list-ref placed j))))
                                                                new-to-place)) }
                            ;; reject a state if it has a row, diagnol or column that does not sum up to 15. 
                            (bs n (add1 i) (if  (ms-sum-invalid? new-placed)  acc (cons new-placed acc))))
                            ;; reject an attempt to create a new state if doing will result in non-solutions.
                            (bs n (add1 i) acc))]
               [else acc]))) }
     (apply append (map (λ (to-place) (bs to-place 0 '())) (ms-state-to-place s)))))


(check-expect (member (make-ms-state (list #f #f #f #f 5 #f #f #f #f) (list 1 2 3 4 6 7 8 9)) (build-state 1-9state)) true)
(check-expect (member (make-ms-state (list 9 1 2 3 4 5 6 7 #f) '(8)) (build-state (make-ms-state (list 9 1 2 3 4 5 6 #f #f) '(7 8)))) false)


;; ms-sum-invalid? : ms-state -> book
;; returns true if some row col or diagnol doesnt add up to 15
;; returns false otherwise
(define (ms-sum-invalid? s)
  (local { (define indices (list (list 0 1 2) (list 3 4 5) (list 6 7 8)
                                     (list 0 3 6) (list 1 4 7) (list 2 5 8)
                                     (list 0 4 8) (list 2 4 6)))
           (define (sum lst)
                 (foldl (λ (n acc) (+ (list-ref (ms-state-placed s) n) acc)) 0 lst))
           (define  (has-false lst)
             (ormap (λ (n) (false? (list-ref (ms-state-placed s) n))) lst))
           (define (invalid lol)
             (map (λ (lst) (if (has-false lst)
                               #f
                               (not (= (sum lst) 15)))) lol)) }
    
    (ormap identity (invalid indices))))

(check-expect (ms-sum-invalid? (make-ms-state (list 1 2 3 #f #f #f #f #f #f) (list 4 5 6 7 8 9))) true)
(check-expect (ms-sum-invalid? (make-ms-state (list 1 2 3 4 5 6 7 8 9) '())) true)
(check-expect (ms-sum-invalid? (make-ms-state (list 2 7 6 9 5 1 4 3 8) '())) false)



;; ms-goal? : ms-state -> bool
;; returns #t if goal has been reached, #f otherwise.
(define (ms-goal? s)
  (if (not (empty? (ms-state-to-place s)))
      #f
      (local { (define indices (list (list 0 1 2) (list 3 4 5) (list 6 7 8)
                                     (list 0 3 6) (list 1 4 7) (list 2 5 8)
                                     (list 0 4 8) (list 2 4 6)))
               (define (sum lst)
                 (foldl (λ (n acc) (+ (list-ref (ms-state-placed s) n) acc)) 0 lst))
               (define (same-sum? lol)
                   (not (ormap (λ (lst)  (not (equal? (sum lst) 15))) lol))) }
        (same-sum? indices))))

(check-expect (ms-goal? (make-ms-state (list 2 7 6 9 5 1 4 3 8) '())) true)
(check-expect (ms-goal? (make-ms-state (list 1 2 3 4 5 6 7 8 9) '())) false)
(check-expect (ms-goal? (make-ms-state (list 1 2 4 5 6 7 8 9) '(3))) false)

(define 1-9state (make-ms-state (make-list 9 false) (build-list 9 (λ(n) (add1 n)))))
(define almoststate (make-ms-state (list #f #f #f #f 5 #f #f #f #f) '(1 2 3 4 6 7 8 9)))
(define notvalidstate (make-ms-state (list 9 8 7 #f #f #f #f #f #f) '(1 2 3 4 5 6)))


;; solutions
(define cp-solution (search (build-vector 16 (λ(n) #t)) cp-next))
(define ms-solution-1-9 (search 1-9state ms-next))

;; === grader's tests ===

;; === Problem 2a
"coin problem solution: " cp-solution
;; === Problem 2b
"magic square solution: " (ms-state-placed ms-solution-1-9)

;; === Problem 3
(check-error (vec-max (vector)))
(check-expect (vec-max (vector 1)) 1)
(check-expect (vec-max (vector 1 0)) 1)
(check-expect (vec-max (vector 0 1)) 1)
(check-expect (vec-max (vector 1 1)) 1)
(check-expect (vec-max (build-vector 1000 identity)) 999)
(check-expect (vec-reduce + 0 (vector 3 4 5)) 12)
(check-expect (vec-reduce * 1 (vector 3 4 5)) 60)
(check-expect (vec-indices false? (vector true false false true))
              (list 1 2))
(check-expect (vec-indices even? (vector 2 3 5 6))
              (list 0 3))

;; === evaluation ===

;; === correctness ===

;; problem 1 (see each student's hw6-p1.pdf)
;; - analysis of set-mem?      10/10
;; - analysis of list->sym-set 10/10

;; problem 2a
;; - definition of cp-state    6/ 6
;; - definition of cp-next     6/ 6
;; - definition of cp-solution 2/ 2

;; problem 2b
;; - definition of ms-state (given, no points)
;; - definition of ms-next          10/10
;; - definition of ms-solution-1-9  2/ 2

;; problem 3
;; - vec-max       6/ 6
;; - vec-reduce    6/ 6
;; - vec-indices   6/ 6

;; _subtotal_      64/64

;; === style + svn ===

;; code layout                       3/ 4 (Some code are too far over to the right.)
;; identifiers are well named        6/ 6
;; program decomposition (helpers)   6/ 6
;; contracts                         6/ 6
;; well-written purposes             6/ 6
;; adequate tests                    6/ 6
;; svn usage                         2/ 2

;; _subtotal_                       35/36

;; _total-score_ 99/100

;; grader: Fan Yang