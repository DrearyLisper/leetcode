#lang racket

(struct re/regexp
  (first-state last-state) #:mutable)

(struct re/state
  (edges is-final?) #:mutable)

(struct re/edge
  (symbol) #:mutable)

(define (new-state)
  (re/state (make-hash) #f))

;; (define (re/or . patterns))

(define (re/match regexp s)
  (define symbols (string->list s))
  (define (match-inner state symbols)
    (cond
     ((empty? symbols) (re/state-is-final? state))
     ((not (hash-has-key? (re/state-edges state) (first symbols))) #f)
     (else
      (define next-state (hash-ref (re/state-edges state) (first symbols)))
      (match-inner next-state (rest symbols)))))
  (match-inner (re/regexp-first-state regexp) symbols))

(define (re/string s)
  (define symbols (string->list s))

  (define (add-symbol state symbol)
    (define next-state (new-state))
    (hash-set! (re/state-edges state) symbol next-state)
    next-state)

  (define (add-symbols state symbols)
    (cond
     ((empty? symbols) state)
     (else
      (define symbol (first symbols))
      (define next-state (add-symbol state symbol))
      (add-symbols next-state (rest symbols)))))

  (define first-state (new-state))
  (define final-state (add-symbols first-state symbols))

  (set-re/state-is-final?! final-state #t)
  (re/regexp first-state final-state))


(re/match
 (re/string "ABC")
 "ABC")
