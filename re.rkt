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

(define alphabet (string->list "abcdefghijklmnopqrstuvwxyz"))

(define (re/string->fa s)
  (define symbols (string->list s))


  (define (add-symbol state symbol)
    (define next-state (new-fa-state))
    (if (eq? symbol #\.)
        (map (lambda (letter) (add-state state letter next-state)) alphabet)
        (add-state state symbol next-state))
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

(define (re/and a b)
  (define first-state (new-fa-state))
  (define last-state (new-fa-state))
  (set-re/fa-state-is-final?! (re/fa-last-state a) #f)
  (set-re/fa-state-is-final?! (re/fa-last-state b) #f)

  (add-state first-state 'eps (re/fa-first-state a))
  (add-state (re/fa-last-state a) 'eps (re/fa-first-state b))
  (add-state (re/fa-last-state b) 'eps last-state)

  (set-re/fa-state-is-final?! last-state #t)

  (re/fa first-state last-state))


(define (re/or a b)
  (define first-state (new-fa-state))
  (define last-state (new-fa-state))

  (set-re/fa-state-is-final?! (re/fa-last-state a) #f)
  (set-re/fa-state-is-final?! (re/fa-last-state b) #f)

  (add-state first-state 'eps (re/fa-first-state a))
  (add-state first-state 'eps (re/fa-first-state b))

  (add-state (re/fa-last-state a) 'eps last-state)
  (add-state (re/fa-last-state b) 'eps last-state)

  (set-re/fa-state-is-final?! last-state #t)

  (re/fa first-state last-state))

(define (re/iter a)
  (add-state (re/fa-last-state a) 'eps (re/fa-first-state a))
  (add-state (re/fa-first-state a) 'eps (re/fa-last-state a))
  a)

(define (re/ndfa->dfa fa)
  (define new-states (make-hash))

  (define (has-state set-of-states)
    (hash-has-key? new-states set-of-states))

  (define (get-state set-of-states)
    (if (hash-has-key? new-states set-of-states)
        (hash-ref new-states set-of-states)
        (let ((new-state (new-fa-state))
              (is-final?
               (set-map set-of-states (lambda (state) (re/fa-state-is-final? state)))))
          (hash-set! new-states set-of-states new-state)
          (set-re/fa-state-is-final?! new-state (foldl (lambda (a b) (or a b)) #f is-final?))
          new-state)))

  (define (closure state)
    (define (closure-inner state clojure)
      (define eps-reachable (get-states state 'eps))
      (unless (set-member? clojure state)
        (set-add! clojure state)
        (for ((s (in-set eps-reachable))) (closure-inner s clojure)))
      clojure)
    (closure-inner state (mutable-set)))

  (define (get-symbols-inner states)
    (set-subtract (apply set-union (set-map states get-symbols)) '(eps)))

  (define (get-next-states set-of-states symbol)
    (apply mutable-set
           (apply set-union
                  (set-map set-of-states
                           (lambda (state) (set->list (get-states state symbol)))))))

  (define first-set-of-states (closure (re/fa-first-state fa)))

  (define (bfs first-set-of-states)
    (define state (get-state first-set-of-states))
    (define symbols (get-symbols-inner first-set-of-states))
    (for ((symbol symbols))
      (define next-set-of-states (get-next-states first-set-of-states symbol))
      (define closure-next-set-of-states (apply mutable-set (apply set-union (set-map next-set-of-states
                                                                                      (lambda (s) (set->list (closure s)))))))
      (define had-state (has-state closure-next-set-of-states))
      (define next-state (get-state closure-next-set-of-states))
      (add-state state symbol next-state)
      (unless had-state
       (bfs closure-next-set-of-states))))

  (bfs first-set-of-states)
  (re/fa (get-state first-set-of-states) #f))

(define (parse regexp)
  (define symbol (first regexp))
  (define is-iter (and (not (empty? (rest regexp))) (eq? #\* (first (rest regexp)))))
  (define fa-symbol (re/iter (re/string->fa (list->string (list symbol)))))
  (define fa (if is-iter (re/iter fa-symbol) fa-symbol))
  (define rest-regexp (if is-iter (rest (rest regexp)) (rest regexp)))
  (if (empty? rest-regexp)
      fa
      (re/and fa (parse rest-regexp))))

(define test-1 (re/and
                (re/string->fa "prefix_")
                (re/or
                 (re/iter (re/string->fa "abc"))
                 (re/string->fa "qwe"))))

(define test-2 (re/iter (re/string->fa "a")))

(define test-3 (re/iter (re/string->fa ".")))


(re/match (re/ndfa->dfa test-1) "prefix_abcabcabc")
(re/match (re/ndfa->dfa test-1) "prefix_qwe")
(re/match (re/ndfa->dfa test-2) "a")
(re/match (re/ndfa->dfa test-3) "qweqweqwe")

(re/match
 (re/ndfa->dfa (parse (string->list "c*a*b")))
 "aab"
 )
