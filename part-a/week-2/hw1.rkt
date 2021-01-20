#lang typed/racket

(: get-year ((Listof Integer) -> Integer))
(define (get-year date) (list-ref date 0))

(: get-month ((Listof Integer) -> Integer))
(define (get-month date) (list-ref date 1))

(: get-day ((Listof Integer) -> Integer))
(define (get-day date) (list-ref date 2))

; Takes two dates and returns true if date1 < date2 and false otherwise.
(: is-older ((Listof Integer) (Listof Integer) -> Boolean))
(define (is-older date1 date2)
  (let ([year1 (get-year date1)]
        [year2 (get-year date2)]
        [month1 (get-month date1)]
        [month2 (get-month date2)]
        [day1 (get-day date1)]
        [day2 (get-day date2)])
    (or (< year1 year2)
        (and (equal? year1 year2)
             (< month1 month2))
        (and (equal? year1 year2)
             (equal? month1 month2)
             (< day1 day2)))))

; Helper function. Takes a date and a month and checks if the date is in the month.
(: is-date-in-month ((Listof Integer) Integer -> Boolean))
(define (is-date-in-month date month) (equal? (get-month date) month))

; Takes a list of dates and a month and returns the number of dates that are in the month.
(: number-in-month ((Listof (Listof Integer)) Integer -> Integer))
(define (number-in-month dates month)
  (if (empty? dates)
      0
      (if (is-date-in-month (first dates) month)
          (+ 1 (number-in-month (rest dates) month))
          (number-in-month (rest dates) month))))

; Takes a list of dates and a list of months and returns the number of dates
; that are in one of the months.
(: number-in-months ((Listof (Listof Integer)) (Listof Integer) -> Integer))
(define (number-in-months dates months)
  (if (empty? months)
      0
      (+ (number-in-month dates (first months))
         (number-in-months dates (rest months)))))

; Takes a list of dates and a month and returns a list of dates that are in the month.
(: dates-in-month ((Listof (Listof Integer)) Integer -> (Listof (Listof Integer))))
(define (dates-in-month dates month)
  (if (empty? dates)
      empty
      (if (is-date-in-month (first dates) month)
          (cons (first dates) (dates-in-month (rest dates) month))
          (dates-in-month (rest dates) month))))

; Takes a list of dates and a list of months and returns a list of dates that
; are in one of the months.
(: dates-in-months ((Listof (Listof Integer)) (Listof Integer) -> (Listof (Listof Integer))))
(define (dates-in-months dates months)
  (if (empty? months)
      empty
      (append (dates-in-month dates (first months)) (dates-in-months dates (rest months)))))

; Helper function. Takes a generic list and an integer n and returns the nth element of the list.
(: get-nth-element (All (T) (Listof T) Integer -> T))
(define (get-nth-element xs n)
  (if (equal? n 1)
      (first xs)
      (get-nth-element (rest xs) (- n 1))))

; Takes a list of strings and an integer n and returns the nth element of the list.
(: get-nth ((Listof String) Integer -> String))
(define (get-nth strings n) (get-nth-element strings n))

; Takes a date and returns the date as a string formatted
; as "<month string> <day>, <year>".
(: date-to-string ((Listof Integer) -> String))
(define (date-to-string date)
  (let ([months '("January"
                  "February"
                  "March"
                  "April"
                  "May"
                  "June"
                  "July"
                  "August"
                  "September"
                  "October"
                  "November"
                  "December")])
    (string-append (get-nth months (get-month date))
                   " "
                   (number->string (get-day date))
                   ", "
                   (number->string (get-year date)))))

; Takes an integer sum and a list of integers and returns the largest number
; of elements, starting from the left, that sum up to less than sum.
(: number-before-reaching-sum (Integer (Listof Integer) -> Integer))
(define (number-before-reaching-sum sum xs)
  (if (>= (first xs) sum)
      0
      (let* ([tail (rest xs)]
             [new-head (+ (first xs) (first tail))])
        (+ 1 (number-before-reaching-sum sum (cons new-head (rest tail)))))))

; Helper function. Returns a list of the number of days in each month with
; a leap year flag.
(: number-of-days (Boolean -> (Listof Integer)))
(define (number-of-days is-leap-year)
  (list 31
        (if is-leap-year 29 28)
        31 30 31 30 31 31 30 31 30 31))

; Takes a day of the year from 1 to 365 and returns the month it occurs in as an integer.
(: what-month (Integer -> Integer))
(define (what-month day-of-year)
  (+ 1 (number-before-reaching-sum day-of-year (number-of-days false))))

; Takes two days and returns a list of months that the days range over.
(: month-range (Integer Integer -> (Listof Integer)))
(define (month-range day1 day2)
  (if (> day1 day2)
      empty
      (cons (what-month day1) (month-range (+ 1 day1) day2))))

; Helper function. Takes a list of dates and returns the oldest date.
(: max-date ((Listof (Listof Integer)) -> (Listof Integer)))
(define (max-date dates)
  (if (empty? (rest dates))
      (first dates)
      (let ([tl-ans (max-date (rest dates))])
        (if (is-older (first dates) tl-ans)
            (first dates)
            tl-ans))))

; Racket doesn't have a built-in option type, so we define our own.
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Option a) (U None (Some a)))

; Takes a list of dates and returns the oldest date as an option type.
(: oldest ((Listof (Listof Integer)) -> (Option (Listof Integer))))
(define (oldest dates)
  (if (empty? dates)
      (None)
      (Some (max-date dates))))

; Helper function. Takes an element and a list and returns true if the
; element is in the list and false otherwise.
(: in-list (Integer (Listof Integer) ->  Boolean))
(define (in-list element xs)
  (if (empty? xs)
      false
      (if (equal? element (first xs))
          true
          (in-list element (rest xs)))))

; Helper function. Takes a list of integers and returns a list with duplicates removed.
(: remove-duplicates2 ((Listof Integer) -> (Listof Integer)))
(define (remove-duplicates2 xs)
  (define (helper (ys : (Listof Integer))) : (Listof Integer)
    (if (empty? ys)
        empty
        (if (in-list (first ys) (rest ys))
            (helper (rest ys))
            (cons (first ys) (helper (rest ys))))))
  (reverse (helper (reverse xs))))

; Is the same as number_in_months but handles duplicates in the list of months.
(: number-in-months-challenge ((Listof (Listof Integer)) (Listof Integer) -> Integer))
(define (number-in-months-challenge dates months)
  (number-in-months dates (remove-duplicates2 months)))

; Is the same as datesInMonths but handles duplicates in the list of months.
(: dates-in-months-challenge ((Listof (Listof Integer)) (Listof Integer) -> (Listof (Listof Integer))))
(define (dates-in-months-challenge dates months)
  (dates-in-months dates (remove-duplicates2 months)))

; Helper function. Takes two integers x and y and returns true if y divides x,
; i.e., if x/y has remainder 0, and false otherwise.
(: divides (Integer Integer -> Boolean))
(define (divides x y) (equal? (remainder x y) 0))

; Helper function. Takes a year and returns true if its a leap year and false otherwise.
; Made into a separate function to make it easier to test.
(: is-leap-year (Integer -> Boolean))
(define (is-leap-year year)
  (or (divides year 400)
      (and (divides year 4)
           (not (divides year 100)))))

; Takes a date and returns true if the date describes a real date and false otherwise.
(: reasonable-date ((Listof Integer) -> Boolean))
(define (reasonable-date date)
  (define (is-good-year (year : Integer)) : Boolean (> year 0))
  (define (is-good-month (month : Integer)) : Boolean (and (<= 1 month)
                                                           (<= month 12)))
  (define (is-good-day (date : (Listof Integer))) : Boolean
    (let ([days-in-month (get-nth-element (number-of-days (is-leap-year (get-year date)))
                                          (get-month date))])
      (and (<= 1 (get-day date))
           (<= (get-day date) days-in-month))))
  (and (is-good-year (get-year date))
       (is-good-month (get-month date))
       (is-good-day date)))
