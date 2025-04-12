(define (var? thing) 
  (and (symbol? thing)
       (eq? #\? 
            (car
             (string->list
              (symbol->string
               thing))))))

(define (new-var)
  (string->symbol
   (string-append
    "?"
    (symbol->string 
     (gensym)))))

;; convert two statements into an a-list if they have the same
;; arity and funcall
(define (alist<-statements s1 s2)
  (cond ((not (eq? (car s1) (car s2))) #f)
        ((not (eq? (count s1) (count s2))) #f)
        (else (cdr (map (lambda (l) 
                          (cons (car l) (cadr l))) 
                        (zip s1 s2))))))

;; take two statements. get the associations between them as an alist,
;; and then viewing these as links between nodes in a graph, build out
;; a final collection of sets.
(define (groups<-alist alist)
  (if (eq? #f alist)
      #f
      (make-disjoint (map (lambda (p)`(,(car p) ,(cdr p))) 
                          (remove (lambda (p) (eq? (car p)
                                                   (cdr p)))
                                  alist)))))

;; are there any places where we're trying to associate
;; two non-variables together? if so, the answer is ambiguous /
;; unresolvable
(define (unamb? groups)
  (if (eq? groups #f)
      #f
      (every? (lambda (group)
                (<= (count (remove var? group)) 1))
              groups)))

;; return the bindings as a group or false if the statements cannot be unified
(define (unify s1 s2)
  (let ((maybe-groups (groups<-alist (alist<-statements s1 s2))))
    (if (unamb? maybe-groups)
        maybe-groups
        #f)))

;; get all the facts that match a provided simple query
;; (facts need to be simple as well!)
(define (matching-facts facts simple-q)
  (remove (lambda (fact) 
            (eq? #f (unify fact simple-q)))
          facts))

;; get all the valid groups from a list of facts and a simple query
(define (valid-groups facts simple-q)
  (map (partial unify simple-q) (matching-facts facts simple-q)))

;; find all the valid group combinations that don't collide
(define (logic-and groups-a groups-b)
  (filter unamb?
          (mapcat
                (lambda (group-a) 
                  (map (lambda (group-b)
                               (map uniq 
                                    (make-disjoint
                                          (append group-a group-b))))
                             groups-b))
                groups-a)))

;; since either group satisfies the or, just pass both through unmodified 
(define (logic-or groups-a groups-b)
  (map uniq (append groups-a groups-b)))

(define (gensym-var? v)
  (if (symbol? v)
      (let ((l (string->list (symbol->string v))))
        (and (eq? (car l) #\?)
             (eq? (cadr l) #\g)
             (not (eq? #f (string->number
                           (list->string
                            (cddr l)))))))
      #f))
