;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graded-lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Ho Yin Cheung CS151 Lab2 John Reppy

;;leap? number -> boolean
;;returns true if inputted year is a leap year; returns false if year is not leap year.
;;only works for the 20th and 21st century
;;(define leap? (lambda (y) ...))
(check-expect (leap? 1944) true)
(check-expect (leap? 1956) true)
(check-expect (leap? 2000) true)
(check-expect (leap? 2001) false)
;;grader: The lambda is not needed here, you can just call the function.
(define leap?
  (lambda (y)
    (or (and (= (remainder y 4) 0) (> (remainder y 100) 0))
        (= (remainder y 400) 0))))

;; a date is a
;; (make-date m d y) where
;; - m is a number in [1, 12],
;; - d is a number in [1, 31], and
;; - y is a number in [1900, 2099]
(define-struct date (month day year))

;;checked-make-date: num num num -> date
;;checks that all arguments all valid before constructing a date object
;;(define checked-make-date (lambda (m d y) ...))
(check-expect (checked-make-date 1 2 1901) (make-date 1 2 1901))
(check-error (checked-make-date 12 1231 1241))

(define checked-make-date
  (lambda (m d y)
    (if (and 
         (and (integer? m) 
              (integer? d) 
              (integer? y))
         (and (<= 1 m 12)
              ;; grader: This doesn't account for days with less than 31 days.
              (<= 1 d 31)
              (<= 1900 y 2099)))
        (make-date m d y)
        (error 'checked-make-date "invalid date"))))

;;date=?: date date -> boolean
;;returns true if dates are equal, false otherwise
;;(define date=? (lambda (d1 d2) ...))
(check-expect (date=? (checked-make-date 1 2 1990) (checked-make-date 1 2 1990)) true)
(check-expect (date=? (checked-make-date 1 3 1990) (checked-make-date 1 2 1990)) false)

(define date=?
  (lambda (d1 d2)
    (and (= (date-day d1) (date-day d2)) 
         (= (date-month d1) (date-month d2)) 
         (= (date-year d1) (date-year d2)))))

;;date<?: date date -> boolean
;;return true if first date is before the second date, false otherwise
;;(define date<? (lambda (d1 d2) ... ))
(check-expect (date<? (checked-make-date 1 3 1990) (checked-make-date 2 3 1990)) true)
(check-expect (date<? (checked-make-date 1 3 1990) (checked-make-date 1 4 1990)) true)
(check-expect (date<? (checked-make-date 1 3 1990) (checked-make-date 1 3 1991)) true)

(define date<?
  (lambda (d1 d2)
    (or (< (date-year d1) (date-year d2)) 
        ;; grader: This doesn't account for the cases where month1 < month2 but
        ;;         year2 < year1.
        (< (date-month d1) (date-month d2)) 
        (< (date-day d1) (date-day d2)))))



;;day-of-week: date -> string
;;finds the day of the week for some date
;;(define day-of-week (lambda (date) ... ))
(check-expect (day-of-week (checked-make-date 10 1 2013)) "Tuesday")
(check-expect (day-of-week (checked-make-date 5 16 2000)) "Tuesday")
(check-expect (day-of-week (checked-make-date 6 26 1942)) "Friday")
(check-expect (day-of-week (checked-make-date 9 26 1942)) "Saturday")


(define day-of-week 
  (lambda (date)
    (output-day
     (remainder 
       (+ (- (date-year date) 1900) 
         (monthly-adjustment (date-month date) (date-year date))
         (date-day date)
         (floor (/ (date-year date) 4)))
       7))))
       
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

;;date->string: date -> string
;;given a date, returns a string representation of the same date
;;(define date->string (lambda (date) ...))
(check-expect (date->string (checked-make-date 10 9 2013)) "Wednesday 9 Oct 2013")
(check-expect (date->string (checked-make-date 9 26 1942)) "Saturday 26 Sep 1942")

(define date->string
  (lambda (date)
    (string-append
     (day-of-week date)
     " "
     (number->string (date-day date))
     " "
     (month-abbreviation (date-month date))
     " "
     (number->string (date-year date)))))

;;month-abbreviation: num -> string
;;outputs a string representation in three letters of a month given 1<= num <= 12
;;auxiliary function for date->string
;;(define month-abbreviation (lambda (m) ...))
(check-expect (month-abbreviation 1) "Jan")
(check-expect (month-abbreviation 2) "Feb")
(check-expect (month-abbreviation 3) "Mar")
(check-expect (month-abbreviation 4) "Apr")
(check-expect (month-abbreviation 5) "May")
(check-expect (month-abbreviation 6) "Jun")
(check-expect (month-abbreviation 7) "Jul")
(check-expect (month-abbreviation 8) "Aug")
(check-expect (month-abbreviation 11) "Nov")
(check-expect (month-abbreviation 12) "Dec")
(define month-abbreviation
  (lambda (m)
    (cond[(= m 1) "Jan"]
         [(= m 2) "Feb"]
         [(= m 3) "Mar"]
         [(= m 4) "Apr"]
         [(= m 5) "May"]
         [(= m 6) "Jun"]
         [(= m 7) "Jul"]
         [(= m 8) "Aug"]
         [(= m 9) "Sep"]
         [(= m 10) "Oct"]
         [(= m 11) "Nov"]
         [(= m 12) "Dec"])))

(check-expect (date->string (make-date  1 1 2013)) "Tuesday 1 Jan 2013")
(check-expect (date->string (make-date  2 1 2013)) "Friday 1 Feb 2013")
(check-expect (date->string (make-date  3 1 2013)) "Friday 1 Mar 2013")
(check-expect (date->string (make-date  4 1 2013)) "Monday 1 Apr 2013")
(check-expect (date->string (make-date  5 1 2013)) "Wednesday 1 May 2013")
(check-expect (date->string (make-date  6 1 2013)) "Saturday 1 Jun 2013")
(check-expect (date->string (make-date  7 1 2013)) "Monday 1 Jul 2013")
(check-expect (date->string (make-date  8 1 2013)) "Thursday 1 Aug 2013")
(check-expect (date->string (make-date  9 1 2013)) "Sunday 1 Sep 2013")
(check-expect (date->string (make-date 10 1 2013)) "Tuesday 1 Oct 2013")
(check-expect (date->string (make-date 11 1 2013)) "Friday 1 Nov 2013")
(check-expect (date->string (make-date 12 1 2013)) "Sunday 1 Dec 2013")
                 
;; ======== evaluation ========

;; === correctness ===
;; leap?                6 / 6
;; checked-make-date    6 /10
;; date<?               3 / 6
;; date=?               6 / 6
;; day-of-week          10/10
;; date->string         6 / 6

;; _subtotal_           37/44

;; === style ===
;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   8/ 8
;; contracts                         8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; _subtotal_                       48/48

; svn used correctly                 4/ 4

;; _total-score_                    89/96

;; grader: Tristan Rasmussen
