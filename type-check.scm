(define (walk form from to)
  (cond ((equal? form from) to)
        ((list? form)
         (map (lambda (e) (walk e from to)) form))
        (else
         form)))


(define (variablize-args lambda-form)
  (let ((args (cadr lambda-form))
        (body (nth lambda-form 2)))
    (reduce (lambda (wip next-sub)
              (walk wip next-sub (string->symbol 
                                  (string-append "?" 
                                                 (symbol->string 
                                                  next-sub)))))
            body
            args)))

;; generates a new set of type facts where variables
;; line up properly
(define (type-facts)
  (let ((if-consq (new-var))
        (if-alt (new-var))
        (eqp (new-var))
        (qp (new-var)))
    `((car pair ,(new-var))
      (cdr pair ,(new-var))
      (cons ,(new-var) ,(new-var) pair)
      (label symbol ,(new-var) nil)
      (if #f ,(new-var) ,if-alt ,if-alt)
      (if #t ,if-consq ,(new-var) ,if-consq)
      (eq? ,(new-var) ,(new-var) #f)
      (eq? ,eqp ,eqp #t)
      (atom? pair #f)
      (atom? nil #t)
      (atom? bool #t)
      (atom? symbol #t)
      (atom? number #t)
      (atom? fn #t)
      (lambda ,(new-var) ,(new-var)
      (quote ,qp ,qp)))))

(define (flatten-if if-form)
  (let ((pred (nth if-form 1))
        (consq (nth if-form 2))
        (alt (nth if-form 3)))
    '()))

(define (remove-gensym-vars result)
  (map (lambda (groups)
         (remove null? (map (lambda (group)
                              (remove gensym-var? group))
                            groups)))
       result))

(let ((groups<- (partial valid-groups (type-facts))))
  (remove-gensym-vars
  (logic-and (groups<- '(eq? ?seq nil ?equal))
             (logic-or (groups<- `(if #t nil ,(new-var) ?result))
                       (logic-and (groups<- `(if #f ,(new-var) ?alt ?result))
                                  (groups<- '(car ?seq ?alt)))))))

