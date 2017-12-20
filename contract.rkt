#lang racket/base

(require racket/contract)

(provide maybe/c list*/c
         define-flat-recursive-contracts)

(define-values (maybe/c/c list*/c/c)
  (values (contract? . -> . contract?)
          (contract? contract? ... list-contract? . -> . contract?)))

; Version of ‘flat-murec-contract’, that also defines the ids.
#;(define-flat-recursive-contracts (id:id ...+) (contract:expr ...+))

(define (maybe/c contract) (or/c #false contract))

#| Intended for proper list contracts, with at least two contracts, the final one a list contract.
   Otherwise: use ‘cons/c’ directly.
   Also relavent: ‘list-contract?’. |#
(define (list*/c contract . contracts)
  (local-require (only-in racket/list empty?))
  (if (empty? contracts)
      contract
      ; Relies on unary case as base case.
      (cons/c contract (apply list*/c contracts))))

(module+ test (require rackunit)
  
  (define-values (maybe-symbol/c
                  symbol-numbers/c
                  boolean-symbol-numbers/c)
    (values (maybe/c symbol?)
            (list*/c          symbol? (listof number?))
            (list*/c boolean? symbol? (listof number?))))
  
  (check-true (andmap contract? (list maybe-symbol/c
                                      symbol-numbers/c
                                      boolean-symbol-numbers/c)))

  (check-true (andmap maybe-symbol/c (list 'a #false)))
  (check-false (ormap maybe-symbol/c (list "a" #true)))
  
  (check-true (andmap symbol-numbers/c (list '(a) '(a 1) '(a 1 2))))
  (check-false (ormap symbol-numbers/c (list '() 'a 1 '(1) '(1 a) '(1 2))))

  (check-true (andmap boolean-symbol-numbers/c (list '(#false a) '(#true a 1) '(#false a 1 2))))
  (check-false (ormap boolean-symbol-numbers/c (append (list '(a) '(a 1) '(a 1 2))
                                                       (list '() 'a 1 '(1) '(1 a) '(1 2))))))

(require (for-syntax syntax/parse racket/base))

(define-syntax define-flat-recursive-contracts
  (syntax-parser [(_ (id:id ...+) (contract:expr ...+))
                  #'(define-values (id ...) (flat-murec-contract ([id contract] ...)
                                                                 (values id ...)))]))
