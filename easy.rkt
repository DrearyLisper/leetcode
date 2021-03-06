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

(define (longest-common-prefix-hash strs)
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

(define (longest-common-prefix strs)
  (define (length-of-common-prefix a b)
    (apply max (cons
                -1
                (for/list
                    ((i a)
                     (j b)
                     (k (range 0 (string-length a)))
                     #:break (not (equal? i j)))
                  k))))
  (define prefix-length (apply
                         min
                         (for/list ((i strs))
                           (length-of-common-prefix (car strs) i))))
  (substring (car strs) 0 (add1 prefix-length)))
      
; https://leetcode.com/problems/valid-parentheses/

(define (is-valid str)
  (define s (string->list str))
  (define (is-open c)
    (member c '(#\( #\[ #\{)))
  (define (is-close c)
    (member c '(#\) #\] #\})))
  (define (does-match a b)
    (or
     (and
      (equal? a #\()
      (equal? b #\)))
     (and
      (equal? a #\[)
      (equal? b #\]))
     (and
      (equal? a #\{)
      (equal? b #\}))))
  (define (is-valid-inner s stack)
    (cond
      ((empty? s) (empty? stack))
      ((is-open (car s)) (is-valid-inner (cdr s) (cons (car s) stack)))
      ((is-close (car s)) (and
                           (not (empty? stack))
                           (does-match (car stack) (car s))
                           (is-valid-inner (cdr s) (cdr stack))))))

  (is-valid-inner s '()))

; https://leetcode.com/problems/merge-two-sorted-lists/

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define (create-list lst)
  (cond ((empty? lst) #f)
        (else 
         (define head (make-list-node (car lst)))
         (set-list-node-next! head (create-list (cdr lst)))
         head)))

(define (merge-two-lists l1 l2)
  (define (take-head lst next)
    (set-list-node-next! lst next)
    lst)
  (cond ((not l1) l2)
        ((not l2) l1)
        (else
         (define l1val (list-node-val l1))
         (define l2val (list-node-val l2))
         (cond ((< l1val l2val) (take-head l1 (merge-two-lists (list-node-next l1) l2)))
               (else (take-head l2 (merge-two-lists l1 (list-node-next l2))))))))

; https://leetcode.com/problems/remove-duplicates-from-sorted-array/

(define (remove-duplicates nums)
  (define (remove-duplicates-inner nums)
    (cond ((empty? (cdr nums)) nums)
          ((equal? (car nums) (car (cdr nums))) (remove-duplicates-inner (cdr nums)))
          (else (cons (car nums) (remove-duplicates-inner (cdr nums))))))
  (set! nums (remove-duplicates-inner nums))
  (length nums))

; https://leetcode.com/problems/add-two-numbers/


(define (add-two-numbers l1 l2)
  (define (add-inner l1 l2 carry)
    (define l1-val (if l1 (list-node-val l1) 0))
    (define l2-val (if l2 (list-node-val l2) 0))
    (define l1-next (if l1 (list-node-next l1) #f))
    (define l2-next (if l2 (list-node-next l2) #f))
    (define val (+ l1-val l2-val carry))      
    (if (and (= val 0) (not l1) (not l2))
        #f
        (list-node (modulo val 10) (add-inner l1-next l2-next (quotient val 10)))))
  (add-inner l1 l2 0))


; https://leetcode.com/problems/longest-substring-without-repeating-characters/

(define (length-of-longest-substring s)
  (define (inner-iteration l r s lengths)
    (define l-val (if (not (empty? l)) (car l) #f))
    (define r-val (if (not (empty? r)) (car r) #f))
    (cond
      ((and (not l-val) (nor r-val)) lengths)
      ((and r-val (not (set-member? s r-val))) (inner-iteration l (cdr r) (set-add s r-val) (cons (add1 (set-count s)) lengths)))
      (l-val (inner-iteration (cdr l) r (set-remove s l-val) lengths))
      (else (raise "Can't happen"))))

  (if (non-empty-string? s)
      (apply max (inner-iteration (string->list s) (string->list s) (set) '()))
      0))

; https://leetcode.com/problems/remove-duplicates-from-sorted-list/

(define (delete-duplicates node)
  (define (delete-inner head head-next)
    (define head-next-next (if head-next (list-node-next head-next) #f))
    (cond
     ((not head-next) (set-list-node-next! head head-next))
     ((eq?
       (list-node-val head)
       (list-node-val head-next)) (delete-inner head head-next-next))
     (else (set-list-node-next! head head-next) (delete-inner head-next head-next)))
    node)

  (cond
   ((not node) #f)
   (else (delete-inner node node))))
