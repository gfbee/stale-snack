#lang racket/base

#| Pattern Matching

 Variants of ‘match’ and ‘match-lambda’, for defaulting to:
   • #false, for predicate-like matching
   • identity, for rewrite transformation rules

 Function versions have names of the form ‘λ-match<>’, emphasizing the public api aspect
  versus implementation details. In particular, ‘match-lambda’ is provided as ‘λ-match’. |#

(provide λ-match
         λ-match/false    match/false
         λ-match/identity match/identity)

#| ToDo
 • ‘#:when’ et al
 • investigate what's going on when hovering over uses of ‘map-matcher’ |#

#| ToDo: Syntax and Runtime error messages |#
#;(match 0 [])
#;(match-lambda [])
; ?: expected more terms starting with any term
;  parsing context: while parsing a clause with a pattern and a result in: ()
#;(match 0)
#;((match-lambda) 0)
#;(define-syntax (m  stx)
    (syntax-case stx () [(_ e clauses ...) #`(match         e       clauses ...)]))
#;(define-syntax (m′ stx)
    (syntax-case stx () [(_ e clauses ...) #`(match/derived e #,stx clauses ...)]))
#;(m 0)
#;(m′0)

(module+ test (require rackunit)

  (define-syntax map-matcher
    (syntax-parser [(_ (λ-matcher:id matcher:id clause ...) a-list:expr)
                    #'(let ([result-λ (map (λ-matcher        clause ...)  a-list)]
                            [result   (map (λ (v) (matcher v clause ...)) a-list)])
                        (check-equal? result-λ result)
                        result)]))
  
  (check-equal? (map-matcher (λ-match/false match/false)               '(a))   '(#false))
  (check-equal? (map-matcher (λ-match/false match/false ['a])          '(a b)) '(#true #false))
  (check-equal? (map-matcher (λ-match/false match/false ['a 1])        '(a b)) '(1     #false))
  (check-equal? (map-matcher (λ-match/false match/false ['a 1] ['b 2]) '(a b)) '(1     2))
  
  (check-equal? (map-matcher (λ-match/identity match/identity)               '(a))     '(a))
  (check-equal? (map-matcher (λ-match/identity match/identity ['a 1])        '(a b))   '(1 b))
  (check-equal? (map-matcher (λ-match/identity match/identity ['a 1] ['b 2]) '(a b c)) '(1 2 c)))

(require (for-syntax syntax/parse racket/base
                     (only-in "syntax.rkt" syntax-tooltip with-renamed-syntax-errors)))
         
(require (only-in racket/match match/derived match-lambda))

(define-syntaxes (λ-match λ-match/false match/false λ-match/identity match/identity)
  (values
   (syntax-parser [(_ clause ...)
                   (syntax-tooltip this-syntax #'(match-lambda clause ...)
                                   "• just match-lambda, with a different name")])
   (syntax-parser [(_ clause ...)
                   (syntax-tooltip this-syntax #'(λ (v) (match/false v clause ...))
                                   "• match-lambda, but that returns #false if no match")])
   (syntax-parser [(_ e:expr [pattern result:expr ...] ...)
                   (syntax-tooltip this-syntax
                                   (with-renamed-syntax-errors "match/false"
                                     #`(match/derived e #,this-syntax
                                                      [pattern #true result ...]
                                                      ...
                                                      [_ #false]))
                                   "• match, but that produces #false if no match")])
   (syntax-parser [(_ clause ...)
                   (syntax-tooltip this-syntax
                                   #'(λ (v) (match/identity v clause ...))
                                   "• match-lambda, but that returns the argument if no match")])
   (syntax-parser [(_ e:expr clause ...)
                   (syntax-tooltip this-syntax
                                   #`(match/derived e #,this-syntax
                                                    clause
                                                    ...
                                                    [v v])
                                   "• match, but that produces the value if no match")])))


