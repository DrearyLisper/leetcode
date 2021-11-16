#lang racket

(struct re/regexp
  (first-state last-state) #:mutable)

(struct re/state
  (edges is-final?) #:mutable)

(struct re/edge
  (symbol) #:mutable)

(define (new-state)
  (re/state (make-hash) #f))

(define (add-edge state edge)
  (define next-state (new-state))
  (hash-set! (re/state-edges state) (re/edge-symbol edge) next-state)
  next-state)

(define (add-edges state edges)
  (cond
   ((empty? edges) state)
   (else
    (define edge (first edges))
    (define next-state (add-edge state edge))
    (add-edges next-state (rest edges)))))

(define (re/symbol symbol)
  (re/edge symbol))

(define (re/string string)
  (map re/symbol (string->list string)))

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

(define (re/new-regex patterns)
  (define first-state (new-state))
  (define final-state (add-edges first-state patterns))
  (set-re/state-is-final?! final-state #t)
  (re/regexp first-state final-state))


(re/match
 (re/new-regex (list (re/symbol #\A) (re/symbol #\B) (re/symbol #\C)))
 "ABC")
