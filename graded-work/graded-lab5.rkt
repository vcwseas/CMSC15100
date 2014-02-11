;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graded-lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Victor Cheung 438907 CMSC 15100 John Reppy
(require 2htdp/image)
(require racket/match)

;;--------------------------------------------------------------
;;--------------------------Definitions-------------------------
;;--------------------------------------------------------------

;; a vcard is a (make-vcard lname fname email tel)
;; where lname, fname, email and tel are strings
(define-struct vcard (lname fname email tel))

;; a vtree is either
;; - empty, or
;; - (make-vtree c lsub rsub) where c is a vcard, lsub and rsub are vtrees
(define-struct vtree (c lsub rsub))

(define ma-vcard (make-vcard "Anthony" "Mark" "ma@uchicago.edu" "123456"))
(define jc-vcard (make-vcard "Cesar" "Julius" "jc@uchicago.edu" "234566"))
(define pa-vcard (make-vcard "Athena" "Pallas" "pa@uchicago.edu" "098765"))
(define bo-vcard (make-vcard "Obama" "Barrack" "ba@uchicago.edu" "092838"))
(define jo-vcard (make-vcard "Obama" "John" "jo@uchicago.edu" "923844"))
(define 1-vt (make-vtree jc-vcard 
                          (make-vtree ma-vcard 
                                      empty
                                      (make-vtree pa-vcard empty empty))
                          (make-vtree bo-vcard empty empty)))

(define vc-vcard (make-vcard "Cheung" "Victor" "vc@hotmail.com" "654321"))
(define mc-vcard (make-vcard "Carleton" "Mark" "jc@gmail.com" "010101"))
(define sp-vcard (make-vcard "Penal" "Spaniard" "sp@gmail.com" "92835"))
(define wf-vcard (make-vcard "Freed" "Wiley" "wf@hotmail.com" "8273656"))
(define 2-vt (make-vtree vc-vcard 
            (make-vtree mc-vcard empty empty) 
            (make-vtree sp-vcard 
                        (make-vtree wf-vcard empty empty) 
                        empty)))


;;--------------------------------------------------------------
;;--------------------------Lab5--------------------------------
;;--------------------------------------------------------------

;; vcard<? : vcard vcard -> bool
;; compare vcard by last name, then first name (and no other fields)

(define (vcard<? v1 v2)
  (cond
    [(string=? (vcard-lname v1) (vcard-lname v2))
     (cond
       [(string=? (vcard-fname v1) (vcard-fname v2)) #f]
       [(string<? (vcard-fname v1) (vcard-fname v2)) #t]
       [else #f])]
    [(string<? (vcard-lname v1) (vcard-lname v2)) #t]
    [else #f]))

(check-expect (vcard<? ma-vcard jc-vcard) true)
(check-expect (vcard<? bo-vcard ma-vcard) false)
(check-expect (vcard<? bo-vcard bo-vcard) false)
(check-expect (vcard<? bo-vcard bo-vcard) false)
(check-expect (vcard<? bo-vcard jo-vcard) true)
(check-expect (vcard<? (make-vcard "Mars" "Veronica" "etc" "etc")
                       (make-vcard "Mars" "Var" "etc" "etc"))
              false)


;; vcard=? : vcard vcard -> bool
;; compare vcard by last name and first name (and no other fields)

(define (vcard=? v1 v2)
  (and (equal? (vcard-fname v1) (vcard-fname v2)) 
       (equal? (vcard-lname v1) (vcard-lname v2))))

(check-expect (vcard=? jo-vcard jo-vcard) true)
(check-expect (vcard=? bo-vcard jo-vcard) false)

;; insert : vcard vtree -> vtree
;; note: when the name on the given vcard is already present in the vtree,
;; the new vcard displaces the old one
              
(define (insert vc vt)
    (if (empty? vt)
      (make-vtree vc empty empty)
      (local { (define vt-card (vtree-c vt))
               (define lsub (vtree-lsub vt))
               (define rsub (vtree-rsub vt)) }
        (cond
          [(vcard=? vc vt-card) (make-vtree vc lsub rsub)]
          [(vcard<? vc vt-card) (make-vtree vt-card (insert vc lsub) rsub)]
          [else (make-vtree vt-card lsub (insert vc rsub))]))))

(check-expect (insert (make-vcard "Bel" "June" "jv@uchicago.edu" "123") 1-vt)
              (make-vtree
               (make-vcard "Cesar" "Julius" "jc@uchicago.edu" "234566")
               (make-vtree
                (make-vcard "Anthony" "Mark" "ma@uchicago.edu" "123456")
                empty
                (make-vtree (make-vcard "Athena" "Pallas" "pa@uchicago.edu" "098765") empty (make-vtree (make-vcard "Bel" "June" "jv@uchicago.edu" "123") empty empty)))
               (make-vtree (make-vcard "Obama" "Barrack" "ba@uchicago.edu" "092838") empty empty)))

(check-expect (insert (make-vcard "Anthony" "Mark" "gibberish" "123") 1-vt)
              (make-vtree
               (make-vcard "Cesar" "Julius" "jc@uchicago.edu" "234566")
               (make-vtree (make-vcard "Anthony" "Mark" "gibberish" "123") empty (make-vtree (make-vcard "Athena" "Pallas" "pa@uchicago.edu" "098765") empty empty))
               (make-vtree (make-vcard "Obama" "Barrack" "ba@uchicago.edu" "092838") empty empty)))
(check-expect (insert mc-vcard (insert wf-vcard (insert sp-vcard (make-vtree vc-vcard empty empty))))
              (make-vtree vc-vcard (make-vtree mc-vcard empty empty) (make-vtree sp-vcard (make-vtree wf-vcard empty empty) empty)))
(check-expect (insert (make-vcard "Best" "" "" "") 
                      (insert (make-vcard "Was" "" "" "") 
                              (insert (make-vcard "The" "" "" "") 
                                      (insert (make-vcard "Of" "" "" "") 
                                              (insert (make-vcard "Times" "" "" "") 
                                                      (make-vtree (make-vcard "It" "" "" "") 
                                                                  empty 
                                                                  empty))))))
              (make-vtree
               (make-vcard "It" "" "" "")
               (make-vtree (make-vcard "Best" "" "" "") empty empty)
               (make-vtree
                (make-vcard "Times" "" "" "")
                (make-vtree
                 (make-vcard "Of" "" "" "")
                 empty
                 (make-vtree (make-vcard "The" "" "" "") empty empty))
                (make-vtree (make-vcard "Was" "" "" "") empty empty))))


;; find : string string vtree -> (+ vcard false)
;; finds and returns the requested Vcard by last name and first name,
;; otherwise if not found returns false. 

(define (find l f vt)
  (if (empty? vt)
      #f
      (local { (define vt-card (vtree-c vt))
               (define lsub (vtree-lsub vt))
               (define rsub (vtree-rsub vt)) 
               (define lfvcard (make-vcard l f "" ""))}
        (cond
          [(vcard=? lfvcard vt-card) vt-card]
          [(vcard<? lfvcard vt-card) (find l f lsub)]
          [else (find l f rsub)]))))

(check-expect (find "Obama" "Barrack" (make-vtree jc-vcard 
                          (make-vtree (make-vcard "Anthony" "Mark" "gibberish" "123") 
                                      (make-vtree pa-vcard empty empty) 
                                      empty)
                          (make-vtree bo-vcard empty empty))) bo-vcard)

(check-expect (find "Nope" "Barrack" (make-vtree jc-vcard 
                          (make-vtree (make-vcard "Anthony" "Mark" "gibberish" "123") 
                                      (make-vtree pa-vcard empty empty) 
                                      empty)
                          (make-vtree bo-vcard empty empty))) false)
(check-expect (find "Was" "" (insert (make-vcard "Was" "" "" "") 
                              (insert (make-vcard "The" "" "" "") 
                                      (insert (make-vcard "Of" "" "" "") 
                                              (insert (make-vcard "Times" "" "" "") 
                                                      (make-vtree (make-vcard "It" "" "" "") 
                                                                  empty 
                                                                  empty)))))) (make-vcard "Was" "" "" ""))

;; vcard-img : num vcard -> image
;; Draw a rectangular image of the vcard. 
;; The given number is the minimum width of the image.
;; Ensure the rectangle is at least that wide.
;; Include all four elements on the image of the card: the first
;; and last names, the email address, and the phone number.
;; Within these constraints, you have freedom to design how a vcard 
;; is visualized; you need not ask us lots of questions about how 
;; precisely they should look.
;; eyeball-tested
(define (vcard-img min-width vc)
  (match vc
    [(vcard lname fname email tel)
     (underlay
      (rectangle min-width 60 "outline" "maroon")
      (above
       (text (string-append fname " " lname) 16 "black")
       (text email 16 "black")
       (text tel 16 "black")))]
    [empty
    (rectangle min-width 60 "outline" "maroon")]))


;; vtree-img : vtree -> image
;; - The image of the empty tree must not be the empty image -- it
;;   must be something visible (we don't care exactly what). In my 
;;   (Adam's) draft implementation, I use 
;;     (rectangle 40 10 "solid" "maroon")
;; - Follow this algorithm to draw the tree: recursively draw both
;;   subtrees; put them beside one another, aligned at the top; draw 
;;   the root vcard above that, where with min width of the root vcard 
;;  is the total width of the images of the two subtrees.
;; eyeball tested
(define (vtree-img vt)
  (match vt
    [(vtree vcard lsub rsub)
     (local { (define subtrees (beside/align "top"
                                             (vtree-img lsub)
                                             (vtree-img rsub)))
              (define min-width (image-width subtrees)) }
       (above 
      (vcard-img min-width vcard)
                 subtrees))]
    [empty (vcard-img 150 vt)]))
;;
;;
;;
;;
;; === some tests===
(check-expect (vcard<? (make-vcard "z" "a" "" "")
                       (make-vcard "y" "a" "" ""))
              false)

(check-expect (vcard<? (make-vcard "y" "a" "" "")
                       (make-vcard "z" "a" "" ""))
              true)

(check-expect (vcard<? (make-vcard "z" "a" "" "")
                       (make-vcard "z" "b" "" ""))
              true)

(check-expect (vcard<? (make-vcard "z" "a" "" "")
                       (make-vcard "z" "a" "" ""))
              false)

;;
(check-expect (vcard=? (make-vcard "z" "a" "" "")
                       (make-vcard "y" "a" "" ""))
              false)

(check-expect (vcard=? (make-vcard "z" "a" "" "")
                       (make-vcard "z" "a" "" ""))
              true)

(check-expect (vcard=? (make-vcard "z" "a" "e1" "")
                       (make-vcard "z" "a" "e2" ""))
              true)
;;
(check-expect (insert (make-vcard "z" "a" "" "") empty)
              (make-vtree (make-vcard "z" "a" "" "") empty empty))


(check-expect (insert (make-vcard "a" "a" "" "")
                      (insert (make-vcard "z" "a" "" "") empty))
              (make-vtree (make-vcard "z" "a" "" "") 
                          (make-vtree (make-vcard "a" "a" "" "") empty empty)
                          empty))

(check-expect (insert (make-vcard "z" "z" "" "")
                      (insert (make-vcard "z" "a" "" "") empty))
              (make-vtree (make-vcard "z" "a" "" "") 
                          empty 
                          (make-vtree (make-vcard "z" "z" "" "") empty empty)))

;;
(define test-tree
  (local {(define (mk s) (make-vcard s s "" ""))}
    (foldr insert empty (map mk (list "a" "c" "b")))))
(check-expect (find "b" "b" test-tree)
              (make-vcard "b" "b" "" ""))
              
(check-expect (find "c" "c" test-tree)
              (make-vcard "c" "c" "" ""))

(check-expect (find "a" "a" test-tree)
              (make-vcard "a" "a" "" ""))

(check-expect (find "x" "x" test-tree)
              false)
;;
(vcard-img 20 (make-vcard "Sally" "Paul" "sally@paul.com" "(555) 887-7766"))
(vcard-img 100 (make-vcard "Sally" "Paul" "sally@paul.com" "(555) 887-7766"))
(vcard-img 300 (make-vcard "Sally" "Paul" "sally@paul.com" "(555) 887-7766"))
;;
(define contacts
  (list (make-vcard "M" "A" "a@a.com" "(555) 666-7777")
        (make-vcard "G" "A" "a@a.com" "(555) 666-7777")
        (make-vcard "H" "A" "a@a.com" "(555) 666-7777")
        (make-vcard "X" "A" "a@a.com" "(555) 666-7777")
        (make-vcard "Z" "A" "a@a.com" "(555) 666-7777")
        (make-vcard "A" "A" "a@a.com" "(555) 666-7777")))

"foldl" (vtree-img (foldl insert empty contacts))
"foldr" (vtree-img (foldr insert empty contacts))

;; === evaluation ===
;; Good job! 
;;
;;

;; === correctness ===

;; vcard<?       / 7
;; vcard=?       / 7
;; insert        /10
;; find          /10
;; vcard-img     / 6
;; vtree-img     /10

;; _subtotal_    50/50

;; === style + svn ===

;; code layout                      5 / 8 lines should be < 80 chars
;; A lot of repeated code in the check-expect's you can define some vcard variable 
;; which you can reuse.
;; identifiers are well named        / 8
;; program decomposition (helpers)   / 8
;; contracts                         / 8
;; well-written purposes             / 8
;; adequate tests                    / 8
;; svn usage                         / 2

;; _subtotal_                        47/50

;; _total-score_   97/100

;; grader: NEDELINA TENEVA