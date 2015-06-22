#lang racket

(provide (all-defined-out))

(define ones (lambda () (cons 1 ones)))

(define (sequence low high stride)
  (if (> low high) null (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) error "list-nth-mod: negative number"]
        [(null? xs) error "list-nth-mod: empty list"]
        [#t (let ([re (remainder n (length xs))]) (car (list-tail xs re)))]))

(define (stream-for-n-steps s n)
      (cond [(= n 0) null]
            [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                (cons (- 0 x) (lambda () (f(+ x 1))))
                (cons x (lambda () (f(+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (lambda () (cons "dan.jpg"
                   (lambda() (cons "dog.jpg" dan-then-dog)))))

(define (stream-add-zero s)
  (letrec ([f (lambda(x)
             (lambda () (cons (cons 0 (car (x))) (stream-add-zero (cdr (s))))))])
    (f s)))

(define (cycle-lists xs ys)
  (letrec ([f (lambda(n)
             (lambda ()
               (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (f (+ n 1)))))]) 
    (f 0)))


(define (vector-assoc v vec)
  (letrec ([f (lambda(n)
             (cond [(equal? (vector-length vec) n) #f]
                    [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v)
                                                    (vector-ref vec n)
                                                    (f (+ n 1)))]
                    [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]   ; cache have n ele each ele:(v1,ans) ans:(v1,v2)
           [index 0]
           [f (lambda (v)
                (letrec ([ans (vector-assoc v cache)])
                  (if ans
                      (cdr ans)
                      (letrec ([new-ans (assoc v xs)])   
                        (begin (set! index (if (= (+ index 1) n) 0 (+ index 1)))
                               (vector-set! cache index (cons v new-ans))
                               new-ans)))))])
    f))
           

  
  