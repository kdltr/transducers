;;; Transducer makers in clojure
;; + map
;; + cat (concatenate)
;; + mapcat (append-map)
;; + filter
;; + remove
;; + take
;; + take-while
;; + take-nth
;; + drop
;; + drop-while
;; - replace
;; - partition-by
;; + partition-all (partition-every)
;; - keep
;; - keep-indexed
;; - map-indexed
;; - distinct
;; + interpose (intersperse)
;; + dedupe (delete-duplicates)
;; + random-sample

;;; Procedures from eggs
;; - ./srfi-1-procs
;; - ./srfi-13-procs
;; - ./srfi-41-procs
;; - ./srfi-113-procs
;; - ./srfi-116-procs
;; - ./srfi-117-procs
;; - ./srfi-127-procs
;; - ./arrays-procs
;; - ./dyn-vector-procs
;; - ./iset-procs
;; - ./lazy-seq-procs

;;; Additionnal transducers libraries
;; - https://github.com/cgrand/xforms

;;; Reduced markers

(define-record-type reduced-value
  (reduced value)
  reduced?
  (value extract-reduced))

(define (ensure-reduced x)
  (if (reduced? x) x (reduced x)))

(define (unreduced x)
  (if (reduced? x) (extract-reduced x) x))

;;; Transducers

(define (unit) values)

(define (map proc)
  (lambda (step finalize)
    (values (lambda (r x) (step r (proc x)))
            finalize)))

(define (concatenate)
  (lambda (step finalize)
    (values (lambda (r x) (foldl step r x))
            finalize)))

(define (append-map proc)
  (compose (map proc) (concatenate)))

(define (filter pred?)
  (lambda (step finalize)
    (values (lambda (r x) (if (pred? x) (step r x) r))
            finalize)))

(define (filter-map proc)
  (compose (map proc) (filter identity)))

(define (remove pred?)
  (filter (lambda (x) (not (pred? x)))))

(define (delete object #!optional (= equal?))
  (remove (lambda (x) (= x object))))

(define (take n)
  (lambda (step finalize)
    (values
      (lambda (r x)
        (if (positive? n)
            (begin (set! n (sub1 n)) (step r x))
            (reduced r)))
      finalize)))

(define (take-while pred?)
  (lambda (step finalize)
    (values (lambda (r x) (if (pred? x) (step r x) (reduced r)))
            finalize)))

(define (take-nth n)
  (lambda (step finalize)
    (let ((i -1))
      (values (lambda (r x)
                (set! i (add1 i))
                (if (zero? (modulo i n))
                    (step r x)
                    r))
              finalize))))

(define (drop n)
  (lambda (step finalize)
    (values (lambda (r x)
              (if (zero? n)
                  (step r x)
                  (begin
                    (set! n (sub1 n))
                    r)))
            finalize)))

(define (drop-while pred?)
  (lambda (step finalize)
    (let ((dropping #t))
      (values (lambda (r x)
                (if (and dropping
                         (pred? x))
                    r
                    (begin
                      (set! dropping #f)
                      (step r x))))
              finalize))))

(define (partition-every n)
  (lambda (step finalize)
    (let ((acc '()))
      (values (lambda (r x)
                (set! acc (cons x acc))
                (if (= n (length acc))
                    (let ((val (reverse acc)))
                      (set! acc '())
                      (step r val))
                    r))
              (lambda (r)
                (if (null? acc)
                    (finalize r)
                    (let ((val (reverse acc)))
                      (set! acc '())
                      (finalize (unreduced (step r val))))))))))

(define (intersperse separator)
  (lambda (step finalize)
    (let ((started #f))
      (values (lambda (r x)
                (if started
                    (let ((next (step r separator)))
                      (if (reduced? next)
                          next
                          (step next x)))
                    (begin
                      (set! started #t)
                      (step r x))))
              finalize))))

(define (delete-duplicates #!optional (= equal?))
  (lambda (step finalize)
    (let ((last (gensym)))
      (values (lambda (r x)
                (if (= x last)
                    r
                    (begin
                      (set! last x)
                      (step r x))))
              finalize))))

(define (random-sample probability)
  (lambda (step finalize)
    (values (lambda (r x)
              (if (<= (pseudo-random-real) probability)
                  (step r x)
                  r))
            finalize)))

;;; Building procedures

(define build/list
  (case-lambda
    (() '())
    ((l) (reverse l)) ; TODO use reverse! ?
    ((l x) (cons x l))))

(define build/vector
  (case-lambda
    (() '())
    ((l) (list->vector (reverse l))) ; TODO use reverse-list->vector ?
    ((l x) (cons x l))))

; TODO build/string
; TODO build/XXXvector (SRFI-4)
; TODO build/generator

;;; Transducing processes

(define transduce/list 
  (case-lambda
    ((xf kons list)
     (transduce/list xf kons (kons) list))
    ((xf kons init list)
     (let-values (((step finalize) (xf kons kons)))
       (let lp ((result init)
                (rest list))
         (if (or (null? rest)
                 (reduced? result))
             (finalize (unreduced result))
             (lp (step result (car rest))
                 (cdr rest))))))))

(define (make-vector-like-transduce length ref)
  (letrec ((this-transduce
    (case-lambda
      ((xf kons vec-like)
       (this-transduce xf kons (kons) vec-like))
      ((xf kons init vec-like)
       (let-values (((step finalize) (xf kons kons))
                    ((len) (length vec-like)))
         (let lp ((i 0)
                  (result init))
           (if (or (= i len)
                   (reduced? result))
               (finalize (unreduced result))
               (lp (add1 i) (step result (ref vec-like i))))))))))
    this-transduce))

(define transduce/string
  (make-vector-like-transduce string-length string-ref))

(define transduce/vector
  (make-vector-like-transduce vector-length vector-ref))

(define transduce/u8vector
  (make-vector-like-transduce u8vector-length u8vector-ref))

(define transduce/s8vector
  (make-vector-like-transduce s8vector-length s8vector-ref))

(define transduce/u16vector
  (make-vector-like-transduce u16vector-length u16vector-ref))

(define transduce/s16vector
  (make-vector-like-transduce s16vector-length s16vector-ref))

(define transduce/u32vector
  (make-vector-like-transduce u32vector-length u32vector-ref))

(define transduce/s32vector
  (make-vector-like-transduce s32vector-length s32vector-ref))

(define transduce/u64vector
  (make-vector-like-transduce u64vector-length u64vector-ref))

(define transduce/s64vector
  (make-vector-like-transduce s64vector-length s64vector-ref))

(define transduce/f32vector
  (make-vector-like-transduce f32vector-length f32vector-ref))

(define transduce/f64vector
  (make-vector-like-transduce f64vector-length f64vector-ref))

(define transduce/generator
  (case-lambda
    ((xf kons gen)
     (transduce/generator xf kons (kons) gen))
    ((xf kons init gen)
     (let-values (((step finalize) (xf kons kons)))
       (let lp ((result init))
         (let ((val (gen)))
           (if (or (eof-object? val)
                   (reduced? result))
               (finalize (unreduced result))
               (lp (step result val)))))))))
