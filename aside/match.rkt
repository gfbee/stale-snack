#lang racket

(provide define/match₁ ‹›)

(module+ test (require rackunit)
  (require racket/match)
  (check-equal? (match '(1 2 3) [(‹› ‹a› ...) (‹› ‹a› ...)]) '(1 2 3)))

(require (only-in racket/match define/match define-match-expander)
         (for-syntax syntax/parse))

; Feature Extension: safer ‘match’, with declared variables and literals, also for result quasiquote.

(define-syntax define/match₁
  (syntax-parser
    [(_ (‹function› ‹formal›:id)
        [‹pattern› ‹result›:expr ...+]
        ...+)
     (syntax/loc this-syntax
       (define/match (‹function› ‹formal›)
         [(‹pattern›) ‹result› ...]
         ...))]))

(begin-for-syntax
  (require (only-in syntax/stx stx-pair? stx-car stx-cdr)
           racket/match)
  (define (handle-atom stx)
    (if (and (syntax? stx) (symbol? (syntax-e stx))
             (regexp-match-exact? #rx"‹.+›" (symbol->string (syntax-e stx))))
        (quasisyntax/loc stx ,#,(case (syntax->datum stx)
                                  [(‹_›) (datum->syntax stx '_)]
                                  [else stx]))
        stx))
  (define pattern-transformer
    (syntax-parser
      [(_ . ‹list-elements-pattern›)
       (quasisyntax/loc this-syntax
         `#,(let loop ([stx #'‹list-elements-pattern›])
              (if (stx-pair? stx)
                  (quasisyntax/loc (if (syntax? stx) stx #'‹list-elements-pattern›)
                    (#,(loop (stx-car stx)) . #,(loop (stx-cdr stx))))
                  (handle-atom stx))))]))
  (define expression-transformer
    (syntax-parser
      [(_ . ‹list-elements-pattern›)
       (quasisyntax/loc this-syntax
         `#,(let loop ([stx #'‹list-elements-pattern›])
              (define-syntax-rule (quasi new-stx)
                (quasisyntax/loc (if (syntax? stx) stx #'‹list-elements-pattern›) new-stx))
              (if (stx-pair? stx)
                  (match (stx-cdr stx)
                    [`(,(app syntax->datum '...) . ,_)
                     (quasi (,@`#,(loop (stx-car stx)) . #,(loop (stx-cdr (stx-cdr stx)))))]
                    [_  (quasi (#,(loop (stx-car stx)) . #,(loop (stx-cdr stx))))])
                  (handle-atom stx))))])))
;
(define-match-expander ‹› pattern-transformer expression-transformer)