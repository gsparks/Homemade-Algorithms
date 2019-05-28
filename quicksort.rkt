#lang racket

(define (qsrt iarr lt)
  (cond
    [(< 1 (length iarr))
     (let (
           [pivot (first iarr)]
           [gt (lambda (l r) (not (or (lt l r) (equal? l r))))])
       (append
        (qsrt (filter (lambda (x) (lt x pivot)) iarr) lt)
        (filter (lambda (x) (equal? x pivot)) iarr)
        (qsrt (filter (lambda (x) (gt x pivot)) iarr) lt)))]
    [else iarr]))

(qsrt '("hello" "world" "hi" "hell") string<?)
(qsrt '(6 5 4 7 4 2 1 8 9 0) <)
