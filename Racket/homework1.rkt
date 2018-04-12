
#lang racket

(provide (all-defined-out))


(define (sequence low high stride)
  (cond
    [(> low high) null]
    [(= low high) (cons low null)]
    [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (str)
         (string-append str suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps stream n)
  (letrec ([iter (lambda (stream ans acc)
                   (let ([pr (stream)])
                     (if (= acc 0)
                         ans
                         (iter (cdr pr) (append ans (list (car pr))) (- acc 1)))))])
    (iter stream null n)))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- 0 x)
                          x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () (if (string=? x "dan.jpg")
                                       (f "dog.jpg")
                                       (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (x) (cons (cons 0 (car x)) (lambda () (f ((cdr x))))))])
    (lambda () (f (stream)))))


(define (cycle-lists xs ys)
  (letrec ([iter (lambda (n)
                   (cons (cons (list-nth-mod xs n)
                               (list-nth-mod ys n))
                         (lambda () (iter (+ n 1)))))])
    (lambda () (iter 0))))


(define (vector-assoc v vec)
  (letrec ([iter (lambda (i)
                   (if (= i (vector-length vec))
                       #f
                       (let ([x (vector-ref vec i)])
                         (if (and (cons? x) (equal? (car x) v))
                             x
                             (iter (+ i 1))))))])
    (iter 0)))

(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [index 0])
    (lambda (v)
      (let ([ans (vector-assoc v memo)])
        (or ans
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (vector-set! memo index new-ans)     
                    (if (= index (- n 1))
                        (set! index 0)
                        (set! index (+ 1 index)))
                    new-ans)                   
                  #f)))))))
