;; standard code walker
(define (walk form from to)
  (cond ((equal? form from) to)
        ((list? form)
         (map (lambda (e) (walk e from to)) form))
        (else
         form)))


;; take a lambda and convert it to one with logic variables
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

;; convert values with their types, except first element in list
;; since thats a function calls
(define (typify-constants code-statement)
  (let ((convert-element
	 (lambda (element)
	   (cond ((number? element) 'number)
		 ((symbol? element) 'symbol)
		 ((boolean? element) element)
		 ((procedure? element) 'fn)
		 ((list? element) (typify-constants element))
		 (else panic)))))
    (if (list? code-statement)
        (cons (car code-statement)
              (if (eq? (car code-statement) 'quote)
                  (list ((if (list? (cadr code-statement))
                            map
                            funcall)
                         convert-element
                         (cadr code-statement)))
                  (map convert-element (cdr code-statement))))
        (convert-element code-statement))))

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
      (lambda ,(new-var) ,(new-var))
      (quote ,qp ,qp)
      (funcall pair) ; these are placeholders for now
      (funcall nil)
      (funcall bool)
      (funcall symbol)
      (funcall number)
      (funcall fn)
      (funcall pair)
      )))

;; convert a lisp statement into a logical statement
(define (logic<- statement cont)
  (if (list? statement)
      (let ((arg1 (nth statement 1))
	    (arg2 (nth statement 2))
	    (arg3 (nth statement 3))
	    (arg1-cont (new-var))
	    (arg2-cont (new-var))
	    (arg3-cont (new-var)))
	(case (car statement)
	  ((if) (logic<-if statement cont)
	   `(logic-and ,(logic<- arg1 arg1-cont)
		       (logic-or (logic-and ,(logic<- arg2 arg2-cont)
					    (groups<-
					     (quote (if #t ,arg2-cont ,(new-var) ,cont))))
				 (logic-and ,(logic<- arg3 arg3-cont)
					    (groups<-
					     (quote (if #f ,(new-var) ,arg3-cont ,cont)))))))
	  ((eq?) `(logic-and ,(logic<- arg1 arg1-cont)
			     ,(logic<- arg2 arg2-cont)
			     (groups<-
			      (quote (eq? ,arg1-cont ,arg2-cont ,cont)))))
	  ((atom?) `(logic-and ,(logic<- arg1 arg1-cont)
			       (grous<-
				(quote (atom? ,arg1-cont ,cont)))))
	  ((car)  `(logic-and ,(logic<- arg1 arg1-cont)
			      (groups<-
			       (quote (car ,arg1-cont ,cont)))))
	  ((cdr)  `(logic-and ,(logic<- arg1 arg1-cont)
			      (groups<-
			       (quote (cdr ,arg1-cont ,cont)))))
	  ((cons)  `(logic-and ,(logic<- arg1 arg1-cont)
			       ,(logic<- arg2 arg2-cont)
			       (groups<-
				(quote (cons ,arg1-cont ,arg2-cont ,cont)))))
	  ((lambda) `(logic-and ,(logic<- (variablize-args
					  (typify-constants
					   statement)) arg1-cont)
				(groups<-
				 (quote (lambda ,arg1-cont ,cont)))))
	  ((label)  `(logic-and ,(logic<- arg1 arg1-cont)
				,(logic<- arg2 arg2-cont)
				(groups<-
				 (quote (label ,arg1-cont ,arg2-cont ,cont)))))
	  ((quote)  ; erm, what are we supposed to do here?
	   `(groups<- (quote (quote ,(if (pair? arg1
						'pair
						arg1))))))
	  (else `(groups<- (quote (funcall ,cont))))))
      statement))

;; remove variables from a results list that were generated by gensyms
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

