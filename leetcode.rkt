#lang racket

; https://leetcode.com/problems/two-sum/

(define (two-sum nums target)
  (define (zip l r)
    (map cons l r))
  (define pairs (zip nums (range 0 (length nums))))
  (define sorted-pairs (list->vector (sort pairs < #:key car)))
  (define (two-sum-inner l r)
    (define lv (car (vector-ref sorted-pairs l)))
    (define rv (car (vector-ref sorted-pairs r)))
    (define li (cdr (vector-ref sorted-pairs l)))
    (define ri (cdr (vector-ref sorted-pairs r)))
    (cond
      ((= l r) (raise "Can't be true"))
      ((= (+ lv rv) target) (list li ri))
      ((> (+ lv rv) target) (two-sum-inner l (sub1 r)))
      ((< (+ lv rv) target) (two-sum-inner (add1 l) r))))
  (two-sum-inner 0 (sub1 (length nums))))

(define (two-sum-bruteforce nums target)
  (define (zip l r)
    (map cons l r))
  (define pairs (zip nums (range 0 (length nums))))
  (car (for*/list
      ((l pairs)
       (r pairs)
        #:when (and
                (not (= (cdr l) (cdr r)))
                (= (+ (car l) (car r)) target)))
    (list (cdr l) (cdr r)))))

