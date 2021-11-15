#lang racket

(struct regexp
  (head-node) #:mutable)

(struct state
  (edges is-final?) #:mutable)

(struct edge
  (symbol) #:mutable)

(define (new-state)
  (state (make-hash) #f))

(define (add-edge state edge)
  (define next-state (new-state))
  (hash-set! (state-edges state) (edge-symbol edge) next-state)
  next-state)

(define (add-edges state edges)
  (cond
   ((empty? edges) state)
   (else
    (define edge (first edges))
    (define next-state (add-edge state edge))
    (add-edges next-state (rest edges)))))

(define (re/symbol symbol)
  (edge symbol))

(define (re/string string)
  (map re/symbol (string->list string)))

(define (re/match edges)
  (define first-state (new-state))
  (define final-state (add-edges first-state edges))
  (define (match s)
    (define symbols (string->list s))
    (define (match-inner state symbols)
      (cond
       ((empty? symbols) (state-is-final? state))
       ((not (hash-has-key? (state-edges state) (first symbols))) #f)
       (else
        (define next-state (hash-ref (state-edges state) (first symbols)))
        (match-inner next-state (rest symbols)))))
    (match-inner first-state symbols))
  (set-state-is-final?! final-state #t)
  match)


((re/match (re/string "ABC")) "ABC")
