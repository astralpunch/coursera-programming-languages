;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out))

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist es)
  (if (null? es)
      (aunit)
      (apair (car es) (racketlist->mupllist (cdr es)))))

;; Problem 2

(define (mupllist->racketlist es)
  (if (aunit? es)
      null
      (cons (apair-e1 es) (mupllist->racketlist (apair-e2 es)))))

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; MUPL interpreter

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(int? e) int e]
        
        [(aunit? e) aunit e]
        
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        
        [(fun? e) (closure env e)]
        
        [(closure? e) closure e]
        
        [(call? e)
         (let ([fun-actual-eval (eval-under-env (call-actual e) env)]
               [closure-eval (eval-under-env (call-funexp e) env)])
           (let ([fun-v (closure-fun closure-eval)]
                 [env-v (closure-env closure-eval)]
                 [fun-name-v (fun-nameopt (closure-fun closure-eval))]
                 [fun-formal (fun-formal (closure-fun closure-eval))]
                 [fun-body-v (fun-body (closure-fun closure-eval))])     
             (if (closure? closure-eval)
                 (if fun-name-v
                     (eval-under-env fun-body-v
                                     (cons (cons fun-formal fun-actual-eval)
                                           (cons (cons fun-name-v closure-eval)
                                                 env-v)))
                     (eval-under-env fun-body-v
                                     (cons (cons fun-formal fun-actual-eval)
                                           env-v)))
               (error "first argument of MUPL function call must be a closure"))))]
        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst must be applied to a pair")))]

        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd must be applied to a pair")))]
        
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))
      
(define (mlet* lstlst e2)
     (if (null? lstlst)
      e2
      (mlet 
       (car (car lstlst)) 
       (cdr (car lstlst))
       (mlet* (cdr lstlst) e2)
       )))


(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))


(define mupl-map
   (fun "map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (call (var "map") (var "f"))
                                  (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

  
