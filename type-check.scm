;; standard code walker
(define (walk form from to)
  (cond ((equal? form from) to)
        ((list? form)
         (map (lambda (e) (walk e from to)) form))
        (else
         form)))

(define (var<-symbol s)
  (string->symbol
   (string-append "?"
                  (symbol->string
                   s))))

;; take a lambda and convert it to one with logic variables
(define (variablize-args lambda-form)
  (let ((args (cadr lambda-form))
        (body (nth lambda-form 2)))
    (reduce (lambda (wip next-sub)
              (walk wip next-sub (var<-symbol next-sub)))
            body
            args)))

;; convert values with their types, except first element in list
;; since thats a function calls
(define (typify-constants code-statement)
  (let ((convert-element
	 (lambda (element)
	   (cond ((number? element) 'number)
         ((var? element) element)
		 ((symbol? element) 'symbol)
		 ((boolean? element) element)
		 ((procedure? element) 'fn)
		 ((list? element) (if (null? element)
                              'nil
                              (typify-constants element)))
		 (else panic)))))
    (if (list? code-statement)
        (if (null? code-statement)
            'nil
        (cons (car code-statement)
              (if (eq? (car code-statement) 'quote)
                  (list (convert-element
                         (cadr code-statement)))
                  (map convert-element (cdr code-statement)))))
        (convert-element code-statement))))

;; generates a new set of type facts where variables
;; line up properly
(define (type-facts)
  (let ((if-consq (new-var))
        (if-alt (new-var))
        (eqp (new-var))
        (qp (new-var))
	(equiv (new-var)))
    `((car pair ,(new-var))
      (cdr pair ,(new-var))
      (cons ,(new-var) ,(new-var) pair)
      (label symbol ,(new-var) nil)
      (if-f #f ,(new-var) ,if-alt ,if-alt)
      (if-t #t ,if-consq ,(new-var) ,if-consq)
      (eq? ,(new-var) ,(new-var) #f)
      (eq? ,eqp ,eqp #t)
      (atom? pair #f)
      (atom? #f #t)
      (atom? #t #t)
      (atom? nil #t)
      (atom? symbol #t)
      (atom? number #t)
      (atom? fn #t)
      (lambda ,(new-var) ,(new-var))
      (quote ,qp ,qp)
      (funcall fn)
      (logic-equiv ,equiv ,equiv))))

;; convert a lisp statement into a logical statement
(define (logic<- statement cont)
  (if (list? statement)
      (let* ((maybe-arg
	      (lambda (n)
		(if (> (count statement) n)
		    (nth statement n)
		    '())))
	     (arg1 (maybe-arg 1))
	     (arg2 (maybe-arg 2))
	     (arg3 (maybe-arg 3))
	     (arg1-cont (new-var))
	     (arg2-cont (new-var))
	     (arg3-cont (new-var))
	     (arity-check (lambda (num-required when-ok)
			    (if (= (- (count statement) 1) num-required)
                                '()
				(print
				 (string-append "arity mismatch: "
						(string<- statement)
						"\n")))
			    (when-ok)))
	     (handle-match-fail (lambda (logic-statement-result)
				  (if (null? logic-statement-result)
				      (print (string-append "type error: "
							    (string<- statement)
							    "\n")))
				  logic-statement-result))
	     (result-check (lambda (num-required body)
			     (handle-match-fail (arity-check num-required body)))))
	(case (car statement)
	  ((if) (result-check
		 3 (lambda ()
		     (logic-and (logic<- arg1 arg1-cont)
				(logic-or (logic-and (logic<- arg2 arg2-cont)
						     (groups<-
						      `(if-t ,arg1-cont ,arg2-cont ,(new-var) ,cont)))
					  (logic-and (logic<- arg3 arg3-cont)
						     (groups<-
						      `(if-f ,arg1-cont ,(new-var) ,arg3-cont ,cont))))))))
	  ((eq?) (result-check
		  2 (lambda ()
		      (logic-and (logic<- arg1 arg1-cont)
				 (logic<- arg2 arg2-cont)
				 
				 (groups<-
				  `(eq? ,arg1-cont ,arg2-cont ,cont))))))
	  ((atom?) (result-check
		    1 (lambda ()
			(logic-and (logic<- arg1 arg1-cont)
				   (groups<-
				    `(atom? ,arg1-cont ,cont))))))
	  ((car)  (result-check
		   1 (lambda ()
		       (logic-and (logic<- arg1 arg1-cont)
				  (groups<-
				   `(car ,arg1-cont ,cont))))))
	  ((cdr)  (result-check
		   1 (lambda ()
		       (logic-and (logic<- arg1 arg1-cont)
				  (groups<-
				   `(cdr ,arg1-cont ,cont))))))
	  ((cons)  (result-check
		    2 (lambda ()
			(logic-and (logic<- arg1 arg1-cont)
				   (logic<- arg2 arg2-cont)
				   (groups<-
				    `(cons ,arg1-cont ,arg2-cont ,cont))))))
	  ((lambda) (result-check
		     2 (lambda ()
			 (logic-and (logic<- (variablize-args
					      (typify-constants
					       statement)) arg1-cont)
				    (groups<-
				     `(lambda ,arg1-cont ,cont))))))
	  ((label)  (result-check
		     2 (lambda ()
			 (logic-and (logic<- arg1 arg1-cont)
				    (logic<- arg2 arg2-cont)
				    (groups<-
				     `(label ,arg1-cont ,arg2-cont ,cont))))))
	  ((quote) (result-check
		    1 (lambda ()
			(groups<- `(quote ,(if (pair? arg1)
					       'pair
					       arg1)
					  ,cont)))))
	  ;; function call
	  (else (handle-match-fail
		 (if (list? (car statement)) ; handle dynamic dispatch
		     (logic<- `(funcall ,@statement) cont)
		     (apply logic-and `((((,cont)))
					,@(if (var? (car statement))
					      `(,(groups<- `(funcall ,(car statement))))
					      '())
					,@(map (lambda (arg) (logic<- arg (new-var)))
					       (cdr statement)))))))))
      (groups<- `(logic-equiv ,statement ,cont))))

;; remove variables from a results list that were generated by gensyms
(define (remove-gensym-vars result)
  (uniq
   (map (lambda (groups)
          (filter (lambda (group) (any? var? group))
		  (remove null? (map (lambda (group)
				       (remove gensym-var? group))
				     groups))))
	result)))

(define (groups<- to-convert) (valid-groups (type-facts) to-convert))

;; takes the choices and groups and reformats as a list of valid type signatures
;; for the various variables.
(define (format-output original-form choices)
  (let* ((add-anys (lambda (group)
		     (if (every? var? group)
			 (cons (gensym) group)
			 group)))
	 (vars  (append (map var<-symbol (cadr  original-form))
			'(?result)))
	 (boolify (lambda (group)
		    (if (any? (partial contains? group) '(#t #f))
			(map (lambda (e)
			       (if (or (eq? e #f) (eq? e #t))
				   'bool
				   e))
			     group)
			group)))
	 (convert-choice
	  (lambda (choice)
	    (let ((with-anys (map add-anys choice)))
	      (map (lambda (var)
		     (cons var
			   (uniq
			    (remove var?
				    (flatten
				     (filter (lambda (g)
					       (contains? g var))
					     with-anys))))))
		   vars)))))
    (map convert-choice choices)))

(define (type-signatures lambda-form)
  (format-output lambda-form
		 (remove-gensym-vars
		  (logic<- 
		   (typify-constants
		    (variablize-args
		     lambda-form))
		   '?result))))

(define (extend-facts facts lambda-form name)
  (append facts (map (partial cons name) (type-signatures lambda-form))))
