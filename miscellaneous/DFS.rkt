;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname DFS) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Graph-Search


;; a graph is a (listof nodes)
;; a node is a (make-node V E)
;; - V is its vertex (vertex name)
;; - E is its outgoing edges (listof other vertices)
(define-struct node (V E))

(define test-graph (list (make-node 'A '(B E))
                         (make-node 'B '(E F))
                         (make-node 'C '(D))
                         (make-node 'D '())
                         (make-node 'E '(C F))
                         (make-node 'F '(D G))
                         (make-node 'G '())))


;; get-Node : graph vertex -> node
(define (get-Node G v)
  (cond
    [(empty? G) '()]
    [(equal? (node-V (car G))  v) (car G)] 
    [else (get-Node (cdr G) v)]))

;; DFS : graph vertex vertex -> boolean
;; tries to find an item starting from a specific vertex
(define (DFS G vs v)
 (local { (define (find edge-lst)
            (cond
              [(empty? edge-lst) #f]
              [(equal? v (car edge-lst)) #t]
              [else
               (local { (define tryedge (find (node-E (get-Node G (car edge-lst))))) }
                 (or tryedge (find (cdr edge-lst))))])) }
   
   (find (node-E (get-Node G vs)))))

;; DFS-path : graph vertex vertex ->  (listof vertex)
;; finds a path between two nodes with the given vertex;
;; if no path, returns empty.
(define (DFS-path G vs vt)
  (local { (define (find edge-lst path)
             (cond
               [(empty? edge-lst) '()]
               [(equal? (car edge-lst) vt) (cons vt path)]
               [else 
                (local { (define p 
                           (find (node-E (get-Node G (car edge-lst))) 
                                 (cons (car edge-lst) path))) }
                  (if (empty? p)
                      (find (cdr edge-lst) path)
                      p))])) }
    (reverse (find (node-E (get-Node G vs)) (list vs)))))








