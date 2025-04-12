;; get all the elements present in s1 and s2
(define (intersect s1 s2)
  (filter (partial contains? s2) s1))

;; merges sets until all that remain are disjoint
(define (make-disjoint sets)
  (if (null? sets)
      '()
      (let* ((should-merge? 
              (lambda (set)
                (not (null? (intersect set (car sets))))))
             (to-merge (filter should-merge? (cdr sets)))
             (not-to-merge (remove should-merge? (cdr sets))))
        (if (null? to-merge)
            (cons (car sets) (make-disjoint (cdr sets)))
            (make-disjoint (cons (apply append (cons (car sets) to-merge))
                                 not-to-merge))))))
