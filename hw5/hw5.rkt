;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 John Reppy 
(require 2htdp/image)
(require racket/match)

;;------------------------------------------------
;;---------------------Definitions----------------
;;------------------------------------------------

;; a (pair α β) is a (make-pair x y)
;; where x is an α, y is a β
(define-struct pair (first second))

;; a (name fname name) is a (make-name fname lname) where
;; - fname is a string
;; - lname is a string
(define-struct name (fname lname))

;; a cct ("color count tree") is either
;; - empty, or
;; - a (make-cct c n lsub rsub) 
;;     where c is a color, n is a number, and lsub and rsub are ccts
(define-struct cct (c n lsub rsub))

(define 100-ran-num (map random (make-list 100 100)))
(define sorted-100-ran-num (quicksort 100-ran-num <))

(define vc (make-name "Victor" "Cheung"))
(define ma (make-name "Mark" "Anders"))
(define mn (make-name "Monica" "Ng"))
(define jn (make-name "Jonathan" "Ng"))
(define xn (make-name "Xanders" "Ng"))
(define MN (make-name "Monica" "Ng"))
(define jh (make-name "Jonathan" "Ho"))
(define name-list (list vc ma mn jn jh xn MN))

(define lst-of-recs (map (λ (n) (rectangle 10 n "solid" "black")) '(500 300 700 200 100 200)))

(define c1 (make-color 0 0 0))
(define c2 (make-color 255 255 255))
(define c3 (make-color 100 100 100))

(define cct-1 (make-cct c3 1 (make-cct c1 1 empty empty) (make-cct c2 1 empty empty)))




;;------------------------------------------------
;;---------------------Problem1-------------------
;;------------------------------------------------

;; select-min : (listof num) -> (pair num (listof num))
;; finds the min of a lst
(define (select-min lst)
  (local { (define min-num (apply min lst)) }
 (make-pair min-num (remove min-num lst))))

(check-expect (select-min '(9 8 7 6 5 4 3 2 1)) 
(make-pair 1 '(9 8 7 6 5 4 3 2)) )
(check-expect (select-min '(1 1 1)) (make-pair 1 '(1 1)))

;; selection-sort : (listof num) -> (sorted listof num)
;; sorts a list into ascending order
(define (selection-sort lst)
  (if (empty? lst)
      empty
      (local { (define p (select-min lst))
               (define min-num (pair-first p))
               (define rest-lst (pair-second p)) }
        (cons min-num (selection-sort rest-lst)))))
(check-expect (selection-sort '( 9 8 7 6 5 4 3 2 1)) '( 1 2 3 4 5 6 7 8 9))
(check-expect (selection-sort '(10 7 9 3 7 8 1)) (quicksort '(10 7 9 3 7 8 1) <))
(check-expect (selection-sort 100-ran-num) sorted-100-ran-num)
(check-expect (selection-sort empty) empty)

;; The time cost of selection-sort will be O(n^2) because it
;; may traverse the entire list to find the minimum number

;;------------------------------------------------
;;---------------------Problem2-------------------
;;------------------------------------------------

;; quicksort-by : (α α -> bool) (listof α) -> (listof α)
;; takes a comparison function and sorts a list of items
;; using the order implied.

(define (quicksort-by comp-proc lst)
  (if (empty? lst) empty
      (local { (define pivot (car lst))
               (define part-lst (partition (λ (n) (comp-proc n pivot)) (cdr lst)))
               (define llst (car part-lst))
               (define rlst (cadr part-lst)) }
        (append (quicksort-by comp-proc llst) 
                (list pivot) 
                (quicksort-by comp-proc rlst)))))

(check-expect (quicksort-by < (list 3 2 1)) (list 1 2 3))
(check-expect (quicksort-by > empty) empty)

;; partition : (α α -> bool) (listof α) -> (listof (listof α) (listof α))
;; divides a list into two sublists, where the first contains elements
;; that satisfy some comparison procedure, and the second contains elements
;; that don't. 
(define (partition comp-proc lst)
  (local { (define (part lst llst rlst)
            (cond
              [(empty? lst) (list llst rlst)]
              [(comp-proc (car lst)) (part (cdr lst) (cons (car lst) llst) rlst)]
              [else (part (cdr lst) llst (cons (car lst) rlst))])) }
    (part lst empty empty)))

;; desc-sort : (listof num) -> (listof num)
;; sorts a list of numers from largest to smallest
(define (desc-sort lst)
  (quicksort-by > lst))
(check-expect  (desc-sort '(1 2 3 4 5)) (list 5 4 3 2 1))
(check-expect (desc-sort 100-ran-num) (quicksort 100-ran-num >))
(check-expect (desc-sort empty) empty)

;; name-sort : (listof name) -> (listof name)
;; sorts a list of names by standard last name then first name order
(define (name-sort lst)
  (quicksort-by (λ (x y) (cond
                           [(string=? (name-lname x) (name-lname y)) 
                            (cond
                              [(string<? (name-fname x) (name-fname y)) true]
                              [(string=? (name-fname x) (name-fname y)) true]
                              [else false])]
                            [(string<? (name-lname x) (name-lname y)) true]
                            [else false])) lst))
(check-expect (name-sort name-list) (list ma vc jh jn MN mn xn))
(check-expect (name-sort empty) empty)

;; img-sort : (listof image) -> (listof image)
;; sorts a list of images from tallest to shortest
(define (img-sort lst)
  (quicksort-by (λ (x y) (cond
                           [(> (image-height x) (image-height y)) true]
                           [else false])) lst))
(check-expect (img-sort lst-of-recs) (map (λ (n) (rectangle 10 n "solid" "black")) 
                                          '(700 500 300 200 200 100)))
(check-expect (img-sort (map (λ (n) (square n "outline" "maroon")) 100-ran-num))
              (map (λ (n) (square n "outline" "maroon")) (desc-sort 100-ran-num)))
(check-expect (img-sort empty) empty)


;;------------------------------------------------
;;---------------------Problem3-------------------
;;------------------------------------------------

          
;; color->rgb-lst : color -> (listof num)
;; converts a color to a list of its components
(define (color->rgb-lst c)
  (list (color-red c) (color-green c) (color-blue c)))

;; color=? : color color -> bool
;; checks if two colors are equal
(define (color=? c1 c2)
  (and ( = (color-red c1) (color-red c2))
       ( = (color-blue c1) (color-blue c2))
       ( = (color-green c1) (color-green c2))))
(check-expect (color=? (make-color 100 200 254) (make-color 100 200 254)) true)
(check-expect (color=? (make-color 100 200 1) (make-color 100 200 2)) false)

;; color<?: (color color -> bool) color color -> bool
;; checks if a color is before another color
;; this is only true if the some component of the first color
;; precedes that of the second, or if all preceding 
;; components are equal and the next component 
;; precedes that of the second. 
(define (color<? c1 c2)
  (local { (define c1-lst (color->rgb-lst c1))
           (define c2-lst (color->rgb-lst c2)) 
           (define (c<? l1 l2)
             (cond
               [(and (empty? l1) (empty? l2)) false]
                [(= (car l1) (car l2)) (c<? (cdr l1) (cdr l2))]
                [else (< (car l1) (car l2))])) }
    (c<? c1-lst c2-lst)))
                                                                                                                                                                                                      
(check-expect (color<? (make-color 100 200 200) (make-color 100 200 200)) false)
(check-expect (color<? (make-color 100 200 199) (make-color 100 200 200)) true)
(check-expect (color<? (make-color 100 199 199) (make-color 100 200 200)) true)
(check-expect (color<? (make-color 99 200 199) (make-color 100 200 200)) true)
(check-expect (color<? (make-color 100 200 199) (make-color 0 200 200)) false)
(check-expect (color<? (make-color 100 199 200) (make-color 100 200 200)) true)


;; cct-insert : color cct -> cct
;; inserts a color into a color coint tree
(define (cct-insert c t)
  (if (empty? t)
      (make-cct c 1 empty empty)
      (local { (define tc (cct-c t))
               (define n (cct-n t))
               (define lsub (cct-lsub t))
               (define rsub (cct-rsub t)) }
        (cond
          [(color=? c tc) (make-cct tc (add1 n) lsub rsub)]
          [(color<? c tc) (make-cct tc n (cct-insert c lsub) rsub)]
          [else (make-cct tc n lsub (cct-insert c rsub))]))))

(check-expect (cct-insert (make-color 50 50 50) cct-1)
              (make-cct c3 1 (make-cct c1 1 empty (make-cct (make-color 50 50 50) 1 empty empty)) (make-cct c2 1 empty empty)))
(check-expect (cct-insert (make-color 50 50 50) empty)
              (make-cct (make-color 50 50 50) 1 empty empty))

;; image->cct : image -> cct
;; builds a cct from an image
(define (image->cct img)
  (local { (define img-lst (image->color-list img))
           (define (l->c lst acc)
             (if (empty? lst)
                 acc
                 (l->c (cdr lst) (cct-insert (car lst) acc)))) }
    (l->c img-lst empty)))

(check-expect (cct-n (image->cct (square 10 "solid" (make-color 255 0 0))))
              100)
(check-expect (image->cct empty-image) empty)

;; color-count : color cct -> num
;; finds the count of a color in the cct
(define (color-count c t)
  (if (empty? t)
      0
      (cond
        [(color=? c (cct-c t)) (cct-n t)]
        [(color<? c (cct-c t)) (color-count c (cct-lsub t))]
        [else (color-count c (cct-rsub t))])))

(check-expect (color-count (make-color 255 0 0) (image->cct (square 10 "solid" (make-color 255 0 0))))
              100)
(check-expect (color-count (make-color 255 0 0) empty) 0)
(check-expect (color-count (make-color 0 0 255) (image->cct (beside
                                                 (square 10 "solid" (make-color 255 0 0))
                                                 (square 10 "solid" (make-color 0 255 0))
                                                 (square 10 "solid" (make-color 0 0 255)))))
              100)
(check-expect (color-count (make-color 255 255 255) (image->cct (beside
                                                 (square 10 "solid" (make-color 255 0 0))
                                                 (square 10 "solid" (make-color 0 255 0))
                                                 (square 10 "solid" (make-color 0 0 255)))))
              0)


;;------------------------------------------------
;;---------------------Problem4-------------------
;;------------------------------------------------

;; run-length encoding works well when lists have long
;; sequences of the same item. It works poorly when neighbouring
;; items are disparate. 

;; rle : (α α -> bool) (listof α) -> (listof (pair α num))
;; uses run-length encoding to compress a list
(define (rle equal-test lst)
  (reverse (rle-aux equal-test empty lst empty)))

(check-expect (rle symbol=? (list 'A 'A 'B 'B 'B 'B 'C 'C 'C)) (list (make-pair 'A 2) (make-pair 'B 4) (make-pair 'C 3)))
(check-expect (rle symbol=? empty) (list empty))

;; rle-aux (X X -> bool) pair (listof X) (listof pairs) -> (listof pairs)
;; auxiliary function for rle
(define (rle-aux equal-test p lst rlst)
  (if (empty? lst)
      (cons p rlst)
  (cond
    [(empty? p) (rle-aux equal-test (make-pair (car lst) 1) (cdr lst) rlst)]
    [(equal-test (pair-first p) (car lst)) (rle-aux equal-test (make-pair (car lst) (add1 (pair-second p))) (cdr lst) rlst)]
    [else  (rle-aux equal-test empty lst (cons p rlst))])))

(check-expect (rle-aux =  empty (list 1 1 2 2 3 3 3) empty) (list (make-pair 3 3) (make-pair 2 2) (make-pair 1 2)))
(check-expect (rle-aux =  empty empty empty) (list empty))


;; rld : (listof (pair α num)) -> (listof α)
;; unpacks an rle-encoded list
(define (rld encoded-list)
  (apply append (map (λ (p) (make-list (pair-second p) (pair-first p))) encoded-list)))
(check-expect (rld (list (make-pair 1 2) (make-pair 2 1) (make-pair 3 3))) (list 1 1 2 3 3 3))
(check-expect (rld (list (make-pair 'A 2) (make-pair 'B 4) (make-pair 'C 3))) (list 'A 'A 'B 'B 'B 'B 'C 'C 'C))


;;------------------------------------------------
;;---------------------Problem5-------------------
;;------------------------------------------------

;; map-image : (color -> color) image -> image
;; processes each pixel of some image
;; eyeball tested
(define (map-image proc img)
  (color-list->bitmap
   (map proc (image->color-list img))
   (image-width img)
   (image-height img)))

;; luminance : color -> num 
;; calculates the luminance of a color
(define (luminance c)
  (+ (* 0.2126 (color-red c)) (* 0.7152 (color-green c)) (* 0.0722 (color-blue c))))
(check-within (luminance (make-color 255 255 255)) 255 0.0001)
(check-within (luminance (make-color 101 105 5)) 96.9296 0.0001)

;; grayscale : image -> image
;; converts an image to grayscale
;; eyeball tested
(define (grayscale img)
  (map-image (λ (c) (local { (define lum (round (luminance c))) }
                      (make-color lum lum lum))) img))

;; negative : image -> image
;; converts an image to its negative
;; eyeball tested
(define (negative img)
  (map-image (λ (c) (make-color (- 255 (color-red c))
                                (- 255 (color-green c))
                                (- 255 (color-blue c)))) img))


;; grayscale-negative : image -> image
;; converts an image to its grayscale negative
;; eyeball tested
(define (grayscale-negative img)
  (map-image (λ (c) (local { (define neg-lum ( round (luminance (make-color (- 255 (color-red c))
                                (- 255 (color-green c))
                                (- 255 (color-blue c)))))) }
                      (make-color neg-lum neg-lum neg-lum))) img))

;; c-grayscale-negative : image -> image
;; done using compose on grayscale and negative
;; inefficient because racket can't optimize and
;; the colorlist is traversed twice. 
;; eyeball tested
(define (c-grayscale-negative img)
  ((compose grayscale negative) img))