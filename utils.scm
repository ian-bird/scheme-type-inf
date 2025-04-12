(define partial 
  (lambda fn-and-args
    (lambda rest-of-args
      (apply (car fn-and-args)
             (append (cdr fn-and-args)
                     rest-of-args)))))

(define (identity x) x)

(define (take n coll)
  (if (> n 0)
      (cons (car coll)
            (take (- n 1) (lazy-cdr coll)))
      '()))

(define (every? f coll) 
  (fold (lambda (e acc) (if acc (f e) #f)) #t coll))

(define (complement f)
  (lambda args
    (if (apply f args)
        #f
        #t)))

(define (any? f coll)
  (not (every? (complement f) coll)))

(define (none? f coll)
  (every? (complement f) coll))

(define (drop n coll)
  (if (> n 0)
      (drop (- n 1) (lazy-cdr coll))
      coll))

(define (prime? n)
  (every? (lambda (l) (not (zero? (modulo n l))))
          (drop 2 (take n (range)))))

(define (remove pred seq)
  (filter (complement pred) seq))

(define (distinct items)
  (define (distinct-tco items acc)
    (if (null? items)
        acc
        (distinct-tco (remove (partial equal? (car items)) (cdr items))
                      (cons (car items) acc))))
  (reverse (distinct-tco items '())))

(define (distinct? items)
  (equal? (distinct items) items))

(define zip
  (lambda lists
    (if (any? null? lists)
        '()
        (cons (map car lists) 
              (apply zip 
                     (map lazy-cdr lists))))))

(define (mapcat proc seq)
  (apply append (map proc seq)))

(define (nth form n)
  (if (= n 0)
      (car form)
      (nth (cdr form) (- n 1))))

(define (reduce proc base seq)
  (if (null? seq)
      base
      (reduce proc (proc base (car seq)) (cdr seq))))

(define (count seq)
  (define (count-tco seq acc)
    (if (null? seq)
        acc
        (count-tco (cdr seq) (+ 1 acc))))
  (count-tco seq 0))

(define (contains? sequence v)
  (any? (partial equal? v) sequence))

(define (uniq seq)
  (define (uniq-tco seq acc)
    (if (null? seq)
        acc
        (uniq-tco (remove (partial equal? (car seq)) (cdr seq)) 
                  (cons (car seq) acc))))
  (reverse (uniq-tco seq '())))

(define (combinations seq1 seq2)
  (mapcat (lambda (e1)
            (map (lambda (e2) (list e1 e2))
                 seq2))
          seq1))

(define (lazy-map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (delay (lazy-map proc (lazy-cdr seq))))))

(define (force-all seq)
  (if (pair? seq)
      (cons (force-all (if (promise? (car seq))
                           (force (car seq))
                           (car seq)))
            (force-all (if (promise? (cdr seq))
                           (force (cdr seq))
                           (cdr seq))))
      (if (promise? seq)
          (force-all (force seq))
          seq)))

(define (reduce-1 proc seq)
  (reduce proc (car seq) (cdr seq)))
