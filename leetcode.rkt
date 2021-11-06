#lang racket

; https://leetcode.com/problems/two-sum

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

; https://leetcode.com/problems/palindrome-number

(define (is-palindrome x)
  (define (digits x)
    (cond ((< x 10) (list x))
          (else (cons (modulo x 10) (digits (quotient x 10))))))
  (define x-digits (digits x))
  (cond ((< x 0) #f)
        (else (equal? x-digits (reverse x-digits)))))

(define (is-palindrome-cheat x)
  (define x-str (string->list (number->string x)))
  (equal? x-str (reverse x-str)))

; https://leetcode.com/problems/longest-common-prefix/

(define (longest-common-prefix strs)
  (define trie (make-hash))
  (define (add-to-trie trie x)
    (cond ((null? x) trie)
          (else
           (let*
               ((current-symb (car x))
                (current-tail (cdr x))
                (current-hash (hash-ref trie current-symb (make-hash))))
             (hash-set! trie current-symb current-hash)
             (add-to-trie current-hash current-tail)))))
  (define (check-in-trie trie x pos) 
    (cond ((null? x) pos)
          ((hash-has-key? trie (car x)) (check-in-trie (hash-ref trie (car x)) (cdr x) (add1 pos)))
          (else pos)))
  
  (add-to-trie trie (string->list (car strs)))
  (define prefix-length (apply min (for/list ((i strs))
                                      (check-in-trie trie (string->list i) 0))))
  (substring (car strs) 0 prefix-length))
