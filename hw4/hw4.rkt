;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; Victor Cheung 438907 CMSC 15100 John Reppy
;;-----------------------Prerequisites-------------------------

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




;; Pascal's Triangle is defined as follows
;; 1 (row 0)
;; 1 1 (row 1)
;; 1 2 1 (row 2)
;; 1 3 3 1 (row 3)
;; 1 4 6 4 1 (row 4)
;; 1 5 10 10 5 1 (row 5)
;; and so on.

;; pascal : num -> (listof (listof num))
;; computes the first n rows of Pascal's triangle,
;; where n is the number of rows. 
(check-expect (pascal 0) empty)
(check-expect (pascal 1) (list (list 1)))
(check-expect (pascal 7) 
              (list
               (list 1)
               (list 1 1)
               (list 1 2 1)
               (list 1 3 3 1)
               (list 1 4 6 4 1)
               (list 1 5 10 10 5 1)
               (list 1 6 15 20 15 6 1)))

(define pascal 
  (λ (n)
    (if (= n 0) 
        empty
        (pascal-aux (- n 1)))))


;; pascal-aux : num -> (listof (listof num))
;; computes the first n+1 rows of Pascal's triangle
;; where n is the number of the final row. 
(check-expect (pascal-aux 6) 
              (list
               (list 1)
               (list 1 1)
               (list 1 2 1)
               (list 1 3 3 1)
               (list 1 4 6 4 1)
               (list 1 5 10 10 5 1)
               (list 1 6 15 20 15 6 1)))

(check-expect (pascal-aux 0) (list (list 1)))

(define pascal-aux
  (λ (r)
    (cond
      [(= r 0) (list '(1))]
      [else (local ((define smaller (pascal-aux (- r 1))))
              (append smaller 
                  (list (tag-one (sum-two (last-of-list smaller))))))])))

;; last-of-list: (listof alpha) -> alpha
;; returns the last element of a list
(check-expect (last-of-list (list 1 2 3 4)) 4)
(check-expect (last-of-list (list 2 -1 3 10)) 10)
(define last-of-list
  (λ (lst)
    (if (empty? (cdr lst)) (car lst)
        (last-of-list (cdr lst)))))

;; tag-one: (listof num) -> (listof num)
;; adds one to both ends of the given list.
(check-expect (tag-one '()) '(1 1))
(check-expect (tag-one '(1 2 3)) '(1 1 2 3 1))
(define tag-one
  (λ (lst)
    (append (cons 1 lst) '(1))))

;; sum-two: (listof num) -> (listof num)
;; creates a list where every element is the sum
;; of two consecutive elements from the given lst,
;; and where an initial list of size 3 would create 
;; a new list of size 2. 
(check-expect (sum-two '(1 3 3 1)) '(4 6 4))
(check-expect (sum-two '(1 4 6 4 1)) '(5 10 10 5))
(check-error (sum-two '()))
(define sum-two
  (λ (lst)
    (cond 
      [(empty? lst) (error "sum-two needs a list of at least one number")]
      [(empty? (cdr lst)) empty]
      [(cons? lst) (cons (+ (car lst) (car (cdr lst))) (sum-two (cdr lst)))])))



;;leap? number -> boolean
;;returns true if inputted year is a leap year; returns false if year is not leap year.
;;only works for the 20th and 21st century
;;(define leap? (lambda (y) ...))
(check-expect (leap? 1944) true)
(check-expect (leap? 1956) true)
(check-expect (leap? 2000) true)
(check-expect (leap? 2001) false)
(define leap?
  (lambda (y)
    (or (and (= (remainder y 4) 0) (> (remainder y 100) 0))
        (= (remainder y 400) 0))))


;;monthly-adjustment num num -> num
;;takes into account month and year and outputs a number for monthly adjustment
;;auxiliary function for day-of-week
;;(define monthly-adjustment (lambda (m y) ...))
(check-expect (monthly-adjustment 1 2000) 0)
(check-expect (monthly-adjustment 1 2001) 1)
(check-expect (monthly-adjustment 2 2000) 3)
(check-expect (monthly-adjustment 2 2001) 4)
(check-expect (monthly-adjustment 3 2000) 4)
(check-expect (monthly-adjustment 4 2000) 0)
(check-expect (monthly-adjustment 6 2000) 5)
(check-expect (monthly-adjustment 7 2000) 0)
(check-expect (monthly-adjustment 8 2000) 3)
(check-expect (monthly-adjustment 9 2000) 6)
(check-expect (monthly-adjustment 11 2000) 4)
(check-expect (monthly-adjustment 12 2000) 6)
(define monthly-adjustment
  (lambda (m y)
    (cond [(and (= m 1) (leap? y)) 0]
          [(and (= m 1) (not (leap? y))) 1]
          [(and (= m 2) (leap? y)) 3]
          [(and (= m 2) (not (leap? y))) 4]
          [(= m 3) 4]
          [(= m 4) 0]
          [(= m 5) 2]
          [(= m 6) 5]
          [(= m 7) 0]
          [(= m 8) 3]
          [(= m 9) 6]
          [(= m 10) 1]
          [(= m 11) 4]
          [(= m 12) 6])))

;;output-day: num -> string
;;takes a number from 0 to 6 and outputs a day of the week accordingly
;;auxiliary function for day-of-week
;;define output-day (lambda (n) ...))
(check-expect (output-day 0) "Sunday")
(check-expect (output-day 1) "Monday")
(check-expect (output-day 3) "Wednesday")
(check-expect (output-day 4) "Thursday")
(define output-day
  (lambda (n)
    (cond [(= n 0) "Sunday"]
          [(= n 1) "Monday"]
          [(= n 2) "Tuesday"]
          [(= n 3) "Wednesday"]
          [(= n 4) "Thursday"]
          [(= n 5) "Friday"]
          [(= n 6) "Saturday"])))



;;month-abbreviation: num -> string
;;outputs a string representation in letters of a month given 1<= num <= 12
;;(define month-abbreviation (lambda (m) ...))
(check-expect (month-abbreviation 1) "January")
(check-expect (month-abbreviation 2) "February")
(check-expect (month-abbreviation 3) "March")
(check-expect (month-abbreviation 4) "April")
(check-expect (month-abbreviation 5) "May")
(check-expect (month-abbreviation 6) "June")
(check-expect (month-abbreviation 7) "July")
(check-expect (month-abbreviation 8) "August")
(check-expect (month-abbreviation 9) "September")
(check-expect (month-abbreviation 10) "October")
(check-expect (month-abbreviation 11) "November")
(check-expect (month-abbreviation 12) "December")
(define month-abbreviation
  (lambda (m)
    (cond[(= m 1) "January"]
         [(= m 2) "February"]
         [(= m 3) "March"]
         [(= m 4) "April"]
         [(= m 5) "May"]
         [(= m 6) "June"]
         [(= m 7) "July"]
         [(= m 8) "August"]
         [(= m 9) "September"]
         [(= m 10) "October"]
         [(= m 11) "November"]
         [(= m 12) "December"])))





;;-------------------------------------------------------------
;;-------------------------Problem1----------------------------
;;-------------------------------------------------------------

;; sum-of-squares : (listof num) -> num
;; add the squares of all numbers in a list
(check-expect (sum-of-square '(1 2 3)) 14)
(check-expect (sum-of-square '()) 0)
(define (sum-of-square lst)
  (foldl (lambda (n l) (+ (expt n 2) l)) 0 lst))

;; within : num (listof posn) -> (list of posn)
;; keep those points whose distance to the origin is 
;; strictly less than the given threshold, discard the rest
(check-expect (within 5 (list (make-posn 3 4)
                            (make-posn 2 3)
                            (make-posn 10 10)
                            (make-posn 1 1)
                            (make-posn 0 0)))
              (list (make-posn 2 3)
                    (make-posn 1 1)
                    (make-posn 0 0)))
(check-expect (within 5 '()) '())
(define (within threshold lst)
  (filter (lambda (p) (< (distance-from-origin p) threshold)) lst))

;; distance-from-origin : posn -> num
;; finds the distance to origin for an ordered pair on a cartesian plane.
(check-expect (distance-from-origin (make-posn 3 4)) 5)
(check-expect (distance-from-origin (make-posn 0 0)) 0)
(define (distance-from-origin p)
  (sqrt (+ (expt (posn-x p) 2)
           (expt (posn-y p) 2))))

;; vec3-sum : (listof vec3) -> vec3
;; computes the sum of all given vectors under vector addition
(check-expect (vec3-sum (list (make-vec3 1 2 3) 
                              (make-vec3 2 3 4) 
                              (make-vec3 1 1 1) 
                              (make-vec3 -1 -1 -1))) 
              (make-vec3 3 5 7))
(check-expect (vec3-sum '()) (make-vec3 0 0 0))
(define (vec3-sum lst)
  (foldl vec3-add (make-vec3 0 0 0) lst))


;; vec3-total-magnitude : (listof vec3) -> num
;; computes the total magnitude of all given vectors
(check-within (vec3-total-magnitude (list (make-vec3 1 2 3) 
                                          (make-vec3 2 3 4) 
                                          (make-vec3 1 1 1) 
                                          (make-vec3 -1 -1 -1))) 
              9.1104335791443 0.0000001)
(check-expect (vec3-total-magnitude '()) 0)
(define (vec3-total-magnitude lst)
  (vec3-mag (vec3-sum lst)))

;; wider-than : num (listof image) -> (listof image)
;; keep the images wider than the given threshold, discard the rest
(check-expect (wider-than 2 (list (rectangle 10 10 "solid" "blue")
                                  (square 5 "solid" "gold")
                                  (circle 1 "solid" "maroon")))
              (list 
               (rectangle 10 10 "solid" "blue")
               (square 5 "solid" "gold")))
(check-expect (wider-than 2 '()) '())
(define (wider-than threshold lst)
  (filter (lambda (img) (> (image-width img) threshold)) lst))


;; total-area : (listof image) -> num
;; using the image-width and image-height of each image in the list, 
;; compute the total area in pixels of all images in the list
(check-expect (total-area (list (rectangle 10 10 "solid" "red")
                                (square 10 "solid" "blue")))
              200)
(check-expect (total-area (list empty-image)) 0)
(define (total-area lst)
  (foldl (lambda (img sum) (+ (* (image-width img) (image-height img)) sum)) 0 lst))



;;--------------------------------------------------------------
;;--------------------------problem2----------------------------
;;--------------------------------------------------------------

;; natfold : (nat alpha -> alpha) alpha nat -> alpha
;; a higher order function for the abstracting natural number functions. 
;; not suitable for check-expects.
;; must implemented towards some purpose to be useful. 
;; the validity of this procedure is proven by the validity
;; of functions that follow its abstraction pattern and is written using it.
(define (natfold proc base n)
  (if (zero? n)
      base
      (proc n (natfold proc base (sub1 n)))))

;; sum-upto : nat -> nat
;; computes the sum of all natural numbers between 0 and n
(check-expect (sum-upto 100) 5050)
(check-expect (sum-upto 0) 0)
(define (sum-upto n)
  (natfold + 0 n))

;; string-upto : nat -> string
;; builds the string of naturals up to the given bound
(check-expect (string-upto 5) "012345")
(check-expect (string-upto 0) "0")
(define (string-upto n)
   (natfold (λ (n string) (string-append string (number->string n))) "0" n))

;; countdown : nat -> (listof nat)
;; build the list of naturals form n down to 0
(check-expect (countdown 5) '(5 4 3 2 1 0))
(check-expect (countdown 0) '(0))
(define (countdown n)
  (natfold cons '(0) n))


;;----------------------------------------------------------------
;;----------------------------problem3----------------------------
;;----------------------------------------------------------------


;; riemann/lr : (num -> num) num num num -> num
;; computes the left-rectangle riemann sum of a function between a and b
;; if the left ending point is greater than the right end point then 
;; the riemann sum is arbitrarily defined as zero.
(check-within (riemann/lr (λ(x) (expt x 2)) 0.001 1 2) 2.33183 0.00001)
(check-expect (riemann/lr (λ(x) (+ x 1)) 0.01 2 1) 0)
(check-within (riemann/lr (λ(x) (cos x)) 0.001 0 3.14) 0 0.1)

(define (riemann/lr f dx a b)
  (if (< b a) 0
  (local ( (define leftpoints (setx a b dx)) )
   (foldl (λ (x sum) (+ (* dx (f x)) sum)) 0 leftpoints))))


;; setx : num num num -> (listof num)
;; finds the left endpoints for the riemann sum calculation
;; if there isn't an exact number of rectangles given by the division of b-a by width
;; then we round off the number of rectangles to be used. This is done with the understanding
;; that the smaller the width of the rectangles the less significant the result of
;; the difference of one rectangle 
;; auxiliary function
(check-expect (setx 1 4 1) '(1 2 3))
(check-error (setx 0 0 0))
(define (setx a b w)
  (if (zero? w) (error "Zero width is impossible. Reenter proper width")
  (local ((define n (ceiling (- (/ (- b a) w) 1))))
    (reverse (natfold (λ (n lst) (cons (+ a (* n w)) lst)) (list a) n)))))



                        
;;----------------------------------------------------------------                        
;;---------------------------problem4-----------------------------
;;----------------------------------------------------------------


;; show-pascal : nat color color -> image
;; renders an image of the first n rows of Pascal's triangle,
;; such that odd numbers are one specified color and even numbers
;; are the other.
;; procedure not suited for check-expects.
;; eyeball-tested. 
(define (show-pascal n odd-color even-color)
  (local ( (define pas (pascal n)) )
    (foldr (λ (lst img) (above (row->text lst odd-color even-color)
                               img))
           empty-image
           pas)))


;; row->text : (listof num) color color -> image
;; rends an image of the list of numbers such that
;; odd numbers are one color and even numbers are another.
;; procedure not suited for check-expects.
;; eyeball tested. 
(define (row->text lst odd-color even-color)
  (local ( (define create-text (λ (n color) (text (string-append (number->string n) " ") 12 color))) )
  (foldr (λ (n img) (cond
                        [(even? n) (beside (create-text n even-color)
                                                 img)]
                        [(odd? n) (beside (create-text n odd-color)
                                                 img)]
                        ))
         empty-image
         lst)))


;; save-show-pascal : num string -> boolean
;; saves an image of the graphics depiction of pascal's triangle
;; because it's so freaking cool. 
(define (save-show-pascal n str)
  (save-image (show-pascal n "gold" "gray") str))


;;---------------------------------------------------------------
;;-----------------------------problem5--------------------------
;;---------------------------------------------------------------

;; cal : num num -> img
;; produces a calendar image for a year and a month in the
;; in the 20th and 21st century
;; not suitable for check-expects
;; eyeball tested
(define (cal month year)
  (if (and (<= 1 month 12) (<= 1900 year 2099))
  (above (text (string-append 
                (month-abbreviation month) 
                " " 
                (number->string year)) 26 "black")
         (row (weekdays-of-month month year)))
  (error "only works for a valid month and a year of the 20th or 21st century")))


;; row: (listof (listof num string)) -> image
;; stacks beside each other cols
(define (row lst-of-days-weekdays)
  (local ( ;; (listof (listof numstrings)) representing the day and its corresponding weekday
           (define mondays (filter-weekdays "Monday" lst-of-days-weekdays))
           (define tuesdays (filter-weekdays "Tuesday" lst-of-days-weekdays))
           (define wednesdays (filter-weekdays "Wednesday" lst-of-days-weekdays))
           (define thursdays (filter-weekdays "Thursday" lst-of-days-weekdays))
           (define fridays (filter-weekdays "Friday" lst-of-days-weekdays))
           (define saturdays (filter-weekdays "Saturday" lst-of-days-weekdays))
           (define sundays (filter-weekdays "Sunday" lst-of-days-weekdays))

           ;; images of the columns for each of the weekdays
           (define col-mon (col mondays))
           (define col-tue (col tuesdays))
           (define col-wed (col wednesdays))
           (define col-thu (col thursdays))
           (define col-fri (col fridays))
           (define col-sat (col saturdays))
           (define col-sun (col sundays))
           (define cols (list
                         col-sun
                         col-mon
                         col-tue
                         col-wed
                         col-thu
                         col-fri
                         col-sat))
           
           ;; the number associated with the first weekday
           (define first-mon (caar mondays))
           (define first-tue (caar tuesdays))
           (define first-wed (caar wednesdays))
           (define first-thu (caar thursdays))
           (define first-fri (caar fridays))
           (define first-sat (caar saturdays))
           (define first-sun (caar sundays))
           (define firsts (list 
                           first-sun
                           first-mon
                           first-tue
                           first-wed
                           first-thu
                           first-fri
                           first-sat))
           
           ;; weekdays abreviations
           (define abvs (list "S" "M" "Tu" "W" "Th" "F" "S"))
           
           ;; modified columns with empty boxes prepended if needed                 
           (define mod-cols (mod-col-pad cols firsts))
           
           ;; modfied columns with weekdays abreviations above the columns.
           (define mod-cols-dayabvs (mod-col-dayabv mod-cols abvs)))
          
 (stack-beside mod-cols-dayabvs)))


;; col: (listof (listof num string)) -> image
;; converts a list of day and weekdays to column of numbered boxes.
;; not suitable for check expects
;; eyeball tested
(define (col lst)
  (foldl (λ (l img) (if (= (image-height img) 0)
                        (box (car l))
                        (underlay/align/offset "left" "bottom"
                                               img
                                               0
                                               60
                                               (box (car l)))))
                          
         empty-image
         lst))

;; filter-weekdays : string (listof (listof num string)) -> (listof (listof num string))
;; creates a list of all the day entries that is a specific day of the week.
(check-expect (filter-weekdays "Wednesday"
                               (list
                                (list 2 "Tuesday")
                                (list 3 "Wednesday")
                                (list 4 "Thursday")))
              (list (list 3 "Wednesday")))

(check-expect (filter-weekdays "Wednesday"
                               '()) 
              empty)
(define (filter-weekdays str lst) (filter (λ (l) (eqv? str (cadr l))) lst)) 

          
;; stack-beside : (listof image) -> image
;; stacks all the columns beside each other
;; not suitable for check-expects
;; eyeball tested
(define (stack-beside lst) 
  (foldr (λ (f r) (beside/align "top" f r)) empty-image lst))



;; mod-col-dayabv: (listof image) (listof string) -> (listof image)
;; prepends the columns with the day of the week abreviations.
;; not suitable for check-expects
;; eyeball tested
(define (mod-col-dayabv mod-cols abvs)
  (map (λ (img str) (above (text str 22 "black")
                           img)) 
       mod-cols 
       abvs))


;; mod-col-pad : (listof image) (listof num) -> (listof image)
;; prepends the columns that need prepending
;; not suitable for check-expects
;; eyeball tested
(define (mod-col-pad cols firsts)
  (cond
    [(empty? firsts) empty]
    [else (cons (pre-mod (car cols) firsts)
                (mod-col-pad (cdr cols) (cdr firsts)))]))



           
;; pre-mod : image (listof num) -> image
;; prepends the column with an empty box if needed
;; not suitable for check-expects
;; eyeball-tested
(define (pre-mod img lst)
  (if (greater-than (car lst) (cdr lst))
      (underlay/align/offset "left" "top"
                             img
                             0 
                             -60
                             (square 50 "outline" "maroon"))
      img))
    


;; greater-than : num (listof num) -> bool
;; returns true if num is greater than another number
;; in the list
(check-expect (greater-than 1 '()) false)
(check-expect (greater-than 2 '(1 2 3 4)) true)
(check-expect (greater-than 4 '(5 6 7 8 1)) true)
(define (greater-than num lst)
  (cond
    [(empty? lst) false]
    [(> num (car lst)) true]
    [else (greater-than num (cdr lst))]))
    

    
;; weekdays-of-month : m y -> (listof (listof num string))
(check-expect (weekdays-of-month 2 1996) (list
 (list 1 "Thursday")
 (list 2 "Friday")
 (list 3 "Saturday")
 (list 4 "Sunday")
 (list 5 "Monday")
 (list 6 "Tuesday")
 (list 7 "Wednesday")
 (list 8 "Thursday")
 (list 9 "Friday")
 (list 10 "Saturday")
 (list 11 "Sunday")
 (list 12 "Monday")
 (list 13 "Tuesday")
 (list 14 "Wednesday")
 (list 15 "Thursday")
 (list 16 "Friday")
 (list 17 "Saturday")
 (list 18 "Sunday")
 (list 19 "Monday")
 (list 20 "Tuesday")
 (list 21 "Wednesday")
 (list 22 "Thursday")
 (list 23 "Friday")
 (list 24 "Saturday")
 (list 25 "Sunday")
 (list 26 "Monday")
 (list 27 "Tuesday")
 (list 28 "Wednesday")
 (list 29 "Thursday")))
(check-error (weekdays-of-month 13 2200))

(define (weekdays-of-month m y)
  (local ( (define lst (days-of-month m y)) )
    (map (λ (n) (cons n (list (week-day m n y)))) lst)))


;; week-day: num num num -> string
;; gives the day of the week for a given date in m,d,y
;; for the 20th and 21st century
(check-expect (week-day 10 25 2013) "Friday")
(check-expect (week-day 11 1 2013) "Friday")
(define week-day
  (lambda (m d y)
    (output-day
     (remainder 
       (+ (- y 1900) 
         (monthly-adjustment m y)
         d
         (floor (/ y 4)))
       7))))



;; days-of-month: num num -> (listof num)
;; for a given year and month returns a list of the days of that month
;; auxiliary function for cal
(check-expect (days-of-month 2 1996) (list 1 2 3 4 5 6 7 8 9 10
                                           11 12 13 14 15 16 17 18 19 20
                                           21 22 23 24 25 26 27 28 29))
(check-expect (days-of-month 3 1996) (list 1 2 3 4 5 6 7 8 9 10
                                           11 12 13 14 15 16 17 18 19 20
                                           21 22 23 24 25 26 27 28 29 30 31))
(define (days-of-month m y)
  (local ( (define list-1-31 (build-list 31 (λ(n) (+ n 1)))) )
  (filter (λ(n) (valid-day m n y)) list-1-31)))



;; valid-day : num num num -> bool
;; checks if a given day of a month and year is a valid day
;; auxiliary function for cal
(check-expect (valid-day 1 1 1990) true)
(check-expect (valid-day 2 29 1995) false)
(check-expect (valid-day 9 31 1995) false)
(check-error (valid-day 19 30 2200))
(define (valid-day m d y)
  (cond
       [(member m (list 9 4 6 11)) (<= 1 d 30)]
       [(and (member m (list 2))
             (leap? y))
         (<= 1 d 29)]
       [(and (member m (list 2))
             (not (leap? y)))
         (<= 1 d 28)]
       [(member m (list 1 3 5 7 8 10 12)) (<= 1 d 31)]
       [else (error "invalid month or year entry")]))



;; box : num -> img
;; produces a numbered box
;; not suitable for check-expects
;; eyeball tested.
(define (box day)
  (underlay (square 50 "outline" "maroon")
            (text (number->string day) 22 "black")))


