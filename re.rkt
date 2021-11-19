#lang racket

(struct re/fa
  (first-state last-state) #:mutable)

(struct re/fa-state
  (edges is-final?) #:mutable)

(define (new-fa-state)
  (re/fa-state (make-hash) #f))

(define (add-state from-state symbol to-state)
  (define to-states (get-states from-state symbol))
  (set-add! to-states to-state)
  (hash-set! (re/fa-state-edges from-state) symbol to-states))

(define (get-states state symbol)
  (define has-to-state (hash-has-key? (re/fa-state-edges state) symbol))
  (if has-to-state (hash-ref (re/fa-state-edges state) symbol) (mutable-set)))

(define (get-symbols state)
  (hash-keys (re/fa-state-edges state)))


(define (re/match fa s)
  (define symbols (string->list s))
  (define (match-inner state symbols)
    (cond
     ((empty? symbols) (re/fa-state-is-final? state))
     ((not (hash-has-key? (re/fa-state-edges state) (first symbols))) #f)
     (else
      (define next-states (get-states state (first symbols)))
      (if (> (set-count next-states) 1)
          (raise "Can't match non-determenistic finite automaton")
          (match-inner (set-first next-states) (rest symbols))))))
  (match-inner (re/fa-first-state fa) symbols))

(define (re/string->fa s)
  (define symbols (string->list s))


  (define (add-symbol state symbol)
    (define next-state (new-fa-state))
    (add-state state symbol next-state)
    next-state)

  (define (add-symbols state symbols)
    (cond
     ((empty? symbols) state)
     (else
      (define symbol (first symbols))
      (define next-state (add-symbol state symbol))
      (add-symbols next-state (rest symbols)))))

  (define first-state (new-fa-state))
  (define last-state (add-symbols first-state symbols))

  (set-re/fa-state-is-final?! last-state #t)
  (re/fa first-state last-state))

(define (re/or a b)
  (set-re/fa-state-is-final?! (re/fa-last-state a) #f)
  (set-re/fa-state-is-final?! (re/fa-last-state a) #f)
  (define first-state (new-fa-state))
  (define last-state (new-fa-state))
  (add-state first-state 'eps (re/fa-first-state a))
  (add-state first-state 'eps (re/fa-first-state b))
  (add-state (re/fa-last-state b) 'eps last-state)
  (add-state (re/fa-last-state b) 'eps last-state)
  (set-re/fa-state-is-final?! last-state #t)
  (re/fa first-state last-state))

(define (re/ndfa->dfa fa)
  (define (closure state)
    (define (closure-inner state clojure)
      (define eps-reachable (get-states state 'eps))
      (set-add! clojure state)
      (for ((s (in-set eps-reachable))) (closure-inner s clojure))
      clojure)
    (closure-inner state (mutable-set)))
  (define (symbols states)
    (set-subtract (apply set-union (set-map states get-symbols)) '(eps)))
  (define (next-states states symbol)
    (apply mutable-set (apply set-union (set-map states (lambda (state) (set->list (get-states state symbol)))))))
  (define first-state (closure (re/fa-first-state fa)))
  (map (lambda (symbol) (cons symbol (next-states first-state symbol))) (symbols first-state)))


(define our-fa (re/or (re/string->fa "A") (re/string->fa "B")))

(re/match our-fa "A")
(re/ndfa->dfa our-fa)
