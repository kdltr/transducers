(module transducers-tests ()
(import scheme (chicken base) (prefix srfi-1 s:) transducers test)

(test-group
  "transduce and build procedures"
  (test '(1 2 3) (transduce/list (map identity) build/list '(1 2 3)))
  (test '#(1 2 3) (transduce/vector (map identity) build/vector '#(1 2 3)))
  (define (generator)
    (let ((v 0)) (lambda () (if (= v 3) '#!eof (begin (set! v (add1 v)) v)))))
  (test '(1 2 3) (transduce/generator (map identity) build/list (generator))))

(test-group "transducers"
(test "partition-every flushing"
      '((1 2) (3))
      (transduce/list (partition-every 2) build/list '(1 2 3)))

(test "take-while early return"
      '(0 1 2 3)
      (transduce/list (take-while (lambda (x) (< x 4))) build/list (s:iota 10)))

(test "early return then flushing"
      '((0 1 2) (3))
      (transduce/list
        (compose (take-while (lambda (x) (< x 4)))
                 (partition-every 3))
        build/list
        (s:iota 10)))

(test "flushing then early return"
      '((0 1) (2 3))
      (transduce/list
        (compose (take-while (lambda (x) (< x 5)))
                 (partition-every 2)
                 (take-while (lambda (l) (< (car l) 4))))
        build/list
        (s:iota 10)))
) ; test-group

(test-exit)
) ; module
