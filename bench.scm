(module bench ()

(import
  scheme
  (chicken base)
  (chicken time)
  (clojurian syntax)
  (prefix srfi-1 s/)
  (prefix transducers t/))

(define num 1000000)
;(define num 100000)
;(define num 10000)
;(define num 1000)

(define primes
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(define (ask-divisible denom)
  (lambda (num)
    (and (not (= num denom))
         (zero? (modulo num denom)))))

(define (title str)
  (newline)
  (print str)
  (print (make-string (string-length str) #\=))
  (newline))

(title "Traditional")

(define (tradi-test nums)
  (s/fold (lambda (p prev) (s/remove (ask-divisible p) prev))
          nums
          primes))

#;(time (tradi-test (s/map add1 (s/iota num))))

(time (s/filter odd? (s/map add1 (s/iota num))))

(title "Transducers")

(define (trans-test)
  (foldl (lambda (r p) (compose r (t/remove (ask-divisible p))))
         (t/unit)
         primes))

#;(time (t/transduce/list
         (compose (t/map add1) (trans-test))
         t/build/list
         (s/iota num)))

(time (t/transduce/list
         (compose (t/map add1)
                  (t/filter odd?))
         t/build/list
         (s/iota num)))

)
