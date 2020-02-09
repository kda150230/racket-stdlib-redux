;; Reimplementation of several Racket standard library functions, including map, filter, and flatten

#lang racket
(provide (all-defined-out))

;; redefines car and cdr for ease of use
;; car/cdr represent the first and last element in a list respectively
(define first car)
(define last cdr)

;; 1: divisible-by-x?
;; Usage: ((divisible-by-x? 5) 25) -> #t
;;        ((divisible-by-x? 3) 10) -> #f
(define (divisible-by-x? num1)
  (lambda (num2)
    (zero? (modulo num2 num1))))

;; 2: function-9 [from outer space]
(define (function-9 func)
  (func 9))

;; 3: my-map
(define (my-map func lst)
  (if (empty? lst)
      empty                         ;; true branch
      (cons (func (first lst))      ;; false branch
        (my-map func (rest lst)))))

(define (combine lst1 lst2)
  (if (empty? lst1)
      empty
      (if (empty? lst2)
          empty
          (cons
            (cons (first lst1) (first lst2))    ;; make a pair from the two lists...
          (combine (rest lst1) (rest lst2)))))) ;; ...and then cons with the other pairs 

;; two helper functions that reimplement filter
;; my-filter2 returns a list of elements that return #f based on the given 'func'
(define (my-filter func lst)
  (if (empty? lst)
      empty
      (if (func (first lst))
          (cons (first lst) (my-filter func (rest lst)))
          (my-filter func (rest lst))
          )))

(define (my-filter2 func lst)
  (if (empty? lst)
      empty
      (if (not(func (first lst)))
          (cons (first lst) (my-filter func (rest lst)))
          (my-filter func (rest lst))
          )))

;; 5: separate
(define (separate func lst)
  (if (empty? lst)
      empty
      (cons (my-filter func lst) (my-filter2 func lst))
      ))

;; 6: is-member?
(define (is-member? expr lst)
  (if (empty? lst)
      #f                                  ;; return false if we cannot find a match
      (if (equal? expr (first lst))
          #t                              ;; return true if we do find a match...
          (is-member? expr (rest lst))))) ;; otherwise test the new head of the list
  
;; 7: my-sorted?
(define (my-sorted? func lst)
  (if (empty? (rest lst))
      #t                                        ;; if we search whole list without finding
      (if (func (first lst) (first (rest lst))) ;; any pairs that break rule, return #t
        (my-sorted? func (rest lst))
        #f)))                                   ;; if two elements aren't sorted, return #f

;; 8: my-flatten
;; Usage: (my-flatten (list 4 5 6 (list 100 200 300))) -> '(4 5 6 100 200 300)
;;        (my-flatten (list 7 (list 8 (list 9 (list 10))))) -> '(7 8 9 10)

(define (my-flatten lst)
  (cond [(empty? lst) empty]                                          ;; return empty if given an empty list
        [(pair? lst)                                                  ;; as long as we find pairs, continue appending the head to our new flattened list
          (append (my-flatten (first lst)) (my-flatten (rest lst)))]
        [else (list lst)]                                             ;; if we encounter a non-list, make it one so we can append it to our flattened list
        ))

;; 9: upper-threshold
(define (upper-threshold lst num)
  (if (empty? lst)
      empty
      (if (< (first lst) num)
          (cons (first lst) (upper-threshold (rest lst) num)) ;; if below threshold number, cons to our return list...
          (upper-threshold (rest lst) num)                    ;; ...otherwise, repeat with the rest of the original list
          )))

;; 10: my-list-ref
(define (my-list-ref lst index)
  (if (empty? lst)
      (error "ERROR: Index out of bounds")
      (if (= index 0)                        ;;we will stop and return the element found when
        (first lst)                          ;;we have looped [index] times
        (my-list-ref (rest lst) (- index 1))
        )))


           