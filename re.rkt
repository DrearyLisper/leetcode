#lang racket

(struct re/fa
  (first-state last-state) #:mutable)

(struct re/fa-state
  (edges is-final?) #:mutable)

(define (new-fa-state)
  (re/fa-state (make-hash) #f))

(define (add-state from-state symbol to-state)
  (define has-to-state (hash-has-key? (re/fa-state-edges from-state) symbol))
  (define to-states (if has-to-state
                        (hash-ref (re/fa-state-edges from-state) symbol)
                        (mutable-set)))
  (set-add! to-states to-state)
  (hash-set! (re/fa-state-edges from-state) symbol to-states))

(define (get-states state symbol)
  (define has-to-state (hash-has-key? (re/fa-state-edges state) symbol))
  (if has-to-state (hash-ref (re/fa-state-edges state)) #f))


(define (re/match fa s)
  (define symbols (string->list s))
  (define (match-inner state symbols)
    (cond
     ((empty? symbols) (re/fa-state-is-final? state))
     ((not (hash-has-key? (re/fa-state-edges state) (first symbols))) #f)
     (else
      (define next-states (hash-ref (re/fa-state-edges state) (first symbols)))
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

;; (define (re/or a b)
;;   (set-re/state-is-final?! (re/regexp-last-state a) #f)
;;   (set-re/state-is-final?! (re/regexp-last-state a) #f)
;;   (define first-state (new-state))
;;   (define last-state (new-state))
;;   )

(re/match
 (re/string->fa "ABC")
 "ABC")
