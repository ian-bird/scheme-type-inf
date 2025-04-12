(define (lazy-cdr pair)
  ((if (promise? (cdr pair))
       force
       identity)
   (cdr pair)))

(define (range)
  (define next (lambda (x) (cons x (delay (next (+ 1 x))))))
  (next 0))

(define (iterate when then)
  (define next (lambda (x) (cons x (delay (next (then x))))))
  (next when))

(define (lazy-map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (delay (lazy-map proc (lazy-cdr seq))))))

(define (lazy-filter pred seq)
  (cond ((null? seq) '())
        ((pred (car seq)) (cons (car seq)
                                (delay (lazy-filter pred (lazy-cdr seq)))))
        (else (lazy-filter pred (lazy-cdr seq)))))

(define (lazy-reject pred seq)
  (lazy-filter (complement pred) seq))

(define (force-all seq)
  (if (null? seq)
      '()
      (cons (car seq) (force-all (lazy-cdr seq)))))

(define (lazy-append s1 s2)
  (let ((forced-s1 (if (promise? s1) (force s1) s1)))
    (if (null? forced-s1)
        s2
        (cons (car forced-s1) (delay (lazy-append (cdr forced-s1) s2))))))

(define (lazy-mapcat proc seq)
  (if (null? seq)
      '()
      (lazy-append (proc (car seq)) (lazy-mapcat proc (lazy-cdr seq)))))
