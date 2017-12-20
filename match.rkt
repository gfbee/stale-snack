#lang racket/base

#| Pattern Matching

 1. Variants of ‘match’ and ‘match-lambda’, for defaulting to:
     • #false, for predicate-like matching
     • identity, for rewrite transformation rules

    Function versions have names of the form ‘λ-match<>’, emphasizing the public api aspect
     versus implementation details. In particular, ‘match-lambda’ is provided as ‘λ-match’.

 2. Pattern maker to bind a particular function of the value to an identifier pattern. |#

(provide λ-match
         λ-match/false    match/false
         λ-match/identity match/identity
         catamorphism)

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

(module+ test (require rackunit))

(module+ test
  
  ; Syntax errors.
  ; Same possibilities as ‘match’, except missing rhs is not an error.
  ;   • a ‘match’ can have no clauses, which will be a runtime error
  (require racket/match)
  #;(match/false ())
  #;(match/false () (_ 1))
  #;(match/false (match)) ; ToDo.
  #;(match/false 0  (()))
  #;(match/false 0  (())   (_ 1))
  #;(match/false 0  (_ 1)  (()))
  #;(match/false 0  (_ ()) (1 1))
  #;(match/false 0  (1 1)  (_ ()))

  ; Runtime errors.
  ; Same possibilities as ‘match’, except of course there will always be a match.
  #;(begin (match/false (/ 0))
           ;  versus:
           (match/false 0)
           ;
           (match/false 0 (1 1) (_ (/ 0)))
           (match/false 0 ((app 2))))

  ; From csc104 language tests.
  (check-equal? (map (λ-match/false [0] [1]) '(0 1 2)) '(#true #true #false))
  (check-equal? (map (λ-match/false [0] [v (list v)]) '(0 1)) '(#true (1)))
  (check-equal? (map (λ-match/false [0] [v (define r (list v)) r]) '(0 1)) '(#true (1)))
  
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

(module+ test
  (require (only-in racket/match match))
  (catamorphism ↓ f)
  (define (f v) (match v
                  [(list (↓ v1) (↓ v2)) (list v1 v2)]
                  [_ (- v)]))
  (define (g v) (catamorphism ↓ g)
    (match v
      [(list (↓ v1) (↓ v2)) (list v2 v1)]
      [_ (* 10 v)]))
  (check-equal? (f '(1 (2 3))) '(-1 (-2 -3)))
  (check-equal? (g '(1 (2 3))) '((30 20) 10)))

(require (for-syntax syntax/parse racket/base
                     (only-in "syntax.rkt" syntax-tooltip with-renamed-syntax-errors)))
         
(require (only-in racket/match match/derived match-lambda))

(define-syntaxes (λ-match λ-match/false match/false match/false′ λ-match/identity match/identity)
  (values
   (syntax-parser [(_ clause ...)
                   (syntax-tooltip this-syntax #'(match-lambda clause ...)
                                   "• just match-lambda, with a different name")])
   (syntax-parser [(_ clause ...)
                   (syntax-tooltip this-syntax #'(λ (v) (match/false v clause ...))
                                   "• match-lambda, but that returns #false if no match")])
   (syntax-parser [(_ e:expr clause ...)
                   (syntax-tooltip this-syntax
                                   (with-renamed-syntax-errors "match" "match/false"
                                     #'(match/false′ e clause ...))
                                   "• match, but that produces #false if no match")])
   (syntax-parser [(_ e:expr
                      [pattern #:when when:expr result:expr ...]
                      clause ...)
                   #`(match/derived e #,this-syntax
                                    [pattern #:when when #true result ...]
                                    [e′ (match/false′ e′ clause ...)])]
                  [(_ e:expr
                      [pattern result:expr ...]
                      clause ...)
                   #`(match/derived e #,this-syntax
                                    [pattern #true result ...]
                                    [e′ (match/false′ e′ clause ...)])]
                  [(_ e:expr)
                   #`(match/derived e #,this-syntax [_ #false])])
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


(require (only-in racket/match define-match-expander))

(define-syntax catamorphism
  (syntax-parser
    [(_ cat-id:id f:expr)
     (syntax-tooltip this-syntax
                     #'(define-match-expander cat-id
                         (syntax-parser [(_ an-id:id) #'(app f an-id)]))
                     (format "~a\n~a"
                             (format "• make match pattern (~a id) to bind id"
                                     (syntax->datum #'cat-id))
                             (format "  to ~s called on the matched value"
                                     (syntax->datum #'f))))]))
