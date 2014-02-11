;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Ho Yin (Victor) Cheung 438907 CS151-aut-13 John Reppy

;;problem1
;;lbkgconvert: number -> number
;;converts from lb to kg
;;(define lbkgconvert (lambda (lb) ...))

(check-within (lbkgconvert 2) 0.907185 0.0001)

(define lbkgconvert
  (lambda (lb) 
    (* lb 0.453592)))

;;kglbconvert: number -> number
;;converts from kg to lb
;;(define kglbconvert (lambda (kg) ...))

(check-within (kglbconvert 2) 4.40925 0.0001)

(define kglbconvert
  (lambda (kg) 
    (* kg 2.20462)))

;;mikmconvert: number -> number
;;converts from mi to km
;;(define mikmconvert (lambda (mi) ...))

(check-within (mikmconvert 2) 3.21869 0.0001)

(define mikmconvert
  (lambda (mi) 
    (* mi 1.60934)))

;;kmmiconvert: number -> number
;;converts from km to mi
;;(define kmmiconvert (lambda (km) ...))

(check-within (kmmiconvert 2) 1.24274 0.0001)

(define kmmiconvert
  (lambda (km) 
    (* km 0.621371)))

;;fccconvert: number -> number
;;converts from fahrenheit to celsius
;;(define fccconvert (lambda f) ...))

(check-within (fcconvert 2) -16.666667 0.0001)

(define fcconvert
  (lambda (f) 
    (* 5/9 (- f 32))))

;;cfcconvert: number -> number
;;converts from celsius to fahrenheit
;;(define cfcconvert (lambda c) ...))

(check-within (cfconvert 2) 35.6 0.0001)

(define cfconvert
  (lambda (c) 
    (+ 32 (* c 9/5))))











;;problem2
;;pythagorean? number number number -> boolean
;;returns true if the three numbers inputted constitute a pythagoren triple, false otherwise.
;;(define pythagorean? (lambda (x y z) ... ))

(check-expect (pythagorean? 3 4 5) true)
(check-expect (pythagorean? 4 5 6) false)

(define pythagorean?
  (lambda (x y z) 
    (= (+ (sqr x) (sqr y)) (sqr z))))











;;problem3
;;weekly-pay: number number -> number
;;finds the weekly pay with wage * 1.5 for all hours > 40
;;(define weekly-pay (lambda (wage hours) ...))

(check-expect (weekly-pay 10 40) 400)
(check-expect (weekly-pay 10 20) 200)
(check-expect (weekly-pay 10 41) 415)

(define weekly-pay
  (lambda (wage hours)
      (if (<= 0 (overtime hours)) 
        (+ (* 40 wage) (* (overtime-wage wage) (overtime hours)))
        (* (- 40 hours) wage))))


;;overtime-wage: number -> number
;;finds the overtime-wage for given wage 
;;(define overtime-wage (lambda (wage) ...))

(check-expect (overtime-wage 10) 15)

(define overtime-wage
  (lambda (wage)
    (* 1.5 wage)))

;;overtime: number -> number
;;finds the number of hours worked overtime, negative if hours < 40
;;(define overtime (lambda (hours) ...))

(check-expect (overtime 41) 1)
(check-expect (overtime 39) -1)

(define overtime
  (lambda (hours)
     (- hours 40)))









;;problem4
;;cab-fare-with-tip: number -> number
;;finds the total payment required for a given cab fare with a set tip rate of 15%
;;rounded upwards to the closest dollar
;;(define cab-fare-with-tip (lambda (fare) ...))

(check-expect (cab-fare-with-tip 100) 115)
(check-expect (cab-fare-with-tip 59) 68)

(define cab-fare-with-tip
  (lambda (fare) 
    (ceiling (* 1.15 fare))))












;;problem5 
;;num-of-digits: number -> number
;;finds the number of digits in a number n^n
;;(define num-of-digits (lambda (n) ...))

(check-expect (num-of-digits 2) 1)
(check-expect (num-of-digits 5) 4)
(check-expect (num-of-digits 10) 11)

(define num-of-digits
  (lambda (n)
    (inexact->exact (round-up (/ (log (expt n n)) (log 10))))))

;;round-up: number -> number
;;finds the next largest integer > n
;;(define round-up (lambda (n) ... ))

(check-expect (round-up 10) 11)
(check-expect (round-up 11.2) 12)

(define round-up
  (lambda (n)
    (if (= (floor n) (ceiling n))
        (+ 1 n)
        (ceiling n))))










;;problem6
;;hemishell-volume: outer-radius thickness -> volume
;;finds the volume of a spherical shell
;;(define hemishell-volume (lambda (radius thickness) ...))

(check-within (hemishell-volume 10 1) 567.58107275 0.0001)

(define hemishell-volume
  (lambda (outer-radius thickness)
      (* 0.5(- (shell-volume outer-radius) 
               (shell-volume (radius-of-smaller-sphere outer-radius thickness))))))

;;radius-of-smaller-sphere: number number -> number
;;finds the radius of the smaller sphere
;;(define radius-of-smaller-sphere (lambda (outer-radius thickness)...))

(check-expect (radius-of-smaller-sphere 12 1) 11)

(define radius-of-smaller-sphere
  (lambda (outer-radius thickness)
    (- outer-radius thickness)))

;;shell-volume: radius -> volume
;;finds the volume of a sphere
;;(define sphere (lambda (radius) ...))

(check-within (shell-volume 10) 4188.79020479 0.0001)

(define shell-volume
  (lambda (radius)
    (* 4/3(* pi(expt radius 3)))))

;;hemishell-area: outer-radius thickness -> area
;;finds the area of a hemishell
;;(define hemishell-area (lambda (radius thickness) ...))

(check-within (hemishell-area 13 5) 1793.849405 0.0001)

(define hemishell-area
  (lambda (outer-radius thickness)
    (+ (* 1/2 (sphere-area outer-radius)) 
       (- (circle-area outer-radius) 
          (circle-area (radius-of-smaller-sphere outer-radius thickness)))
       (* 1/2 (sphere-area (radius-of-smaller-sphere outer-radius thickness))))))

;;sphere-area: radius -> area
;;finds the area of a sphere
;;(define sphere-area (lambda (radius) ...))

(check-within (sphere-area 10) 1256.63706144 0.0001)

(define sphere-area
  (lambda (radius)
    (* 4 (* pi (expt radius 2)))))

;;circle-area: radius -> area
;;finds the area of a circle
;;(define circle-area (lambda (radius)...))

(check-within (circle-area 10) 314.159265 0.0001)

(define circle-area
  (lambda (radius)
    (* pi (expt radius 2))))












;;problem7
(require 2htdp/image)

;;draw-norway-flag: number -> image 
;;draws the flag of norway given some arbitrary width
;;(define draw-norway-flag (lambda (width ...))

;;eyeball-tested

(define draw-norway-flag
  (lambda (width)
    (underlay/align "right" "center"
                    (underlay/align/offset "left" "center"
                     (underlay/align "left" "center"
                      (rectangle width (* 5/6 width) "solid" "red")
                      (rectangle width (* 1/5 width) "solid" "white"))
                     (* 7/30 width) 0
                     (underlay/align "center" "center"
                      (rectangle (* 1/5 width) (* 5/6 width) "solid" "white")
                      (rectangle (* 1/10 width) (* 5/6 width) "solid" "Midnight Blue")))
                     

                    (underlay/align "right" "center"
                     (rectangle (* 1/3 width) (* 5/6 width) "solid" "red")
                     (rectangle (* 1/3 width) (* 1/5 width) "solid" "white")
                     (rectangle width (* 1/10 width) "solid" "Midnight blue")))))



;;draw-iceland-flag
;;draws the flag of iceland given some arbitrary width
;;(define draw-iceland-flag (lambda (width) ...))

;;eyeball-tested

(define draw-iceland-flag
  (lambda (width)
     (underlay/align "right" "center"
                    (underlay/align/offset "left" "center"
                     (underlay/align "left" "center"
                      (rectangle width (* 5/6 width) "solid" "blue")
                      (rectangle width (* 1/5 width) "solid" "white"))
                     (* 7/30 width) 0
                     (underlay/align "center" "center"
                      (rectangle (* 1/5 width) (* 5/6 width) "solid" "white")
                      (rectangle (* 1/10 width) (* 5/6 width) "solid" "red")))
               
                    (underlay/align "right" "center"
                     (rectangle (* 1/3 width) (* 5/6 width) "solid" "blue")
                     (rectangle (* 1/3 width) (* 1/5 width) "solid" "white")
                     (rectangle width (* 1/10 width) "solid" "red")))))














;;problem8
;;wider? image image -> boolean
;;compares the width of flags and returns true when the first image is wider than the second. 
;;(define wider? (lambda (image1 image2) ...))

(check-expect (wider? (rectangle 20 10 "solid" "blue") 
                      (rectangle 10 10 "solid" "black"))
              true)

(define wider? 
  (lambda (image1 image2)
    (> (image-width image1) (image-width image2))))

;;narrower? image image -> boolean
;;compares the width of flags and returns true when the first image is narrower than the second. 
;;(define narrower? (lambda (image1 image2) ...))

(check-expect (narrower? (rectangle 20 10 "solid" "blue") 
                      (rectangle 10 10 "solid" "black"))
              false)

(define narrower?
  (lambda (image1 image2)
    (not (wider? image1 image2))))






