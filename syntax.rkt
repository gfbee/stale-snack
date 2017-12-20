#lang racket/base

(provide syntax-tooltip with-renamed-syntax-errors)

(define-values (syntax/tooltip/c
                with-renamed-syntax-errors/c)
  (values (syntax? syntax? string? . -> . syntax?)
          (string? syntax?         . -> . syntax?)))


(define (syntax-tooltip source-syntax stx message-string)
  (syntax-parse source-syntax [(head:id . _)
                               (syntax-property stx 'mouse-over-tooltips
                                                (vector source-syntax
                                                        (syntax-position source-syntax)
                                                        (sub1 (+ (syntax-position #'head)
                                                                 (syntax-span #'head)))
                                                        message-string))]))

(define (with-renamed-syntax-errors original-name name stx)
  (local-require (only-in racket ~a))
  (with-handlers ([exn:fail:syntax?
                   (λ (e) (raise (make-exn:fail:syntax
                                  (regexp-replace (regexp-quote (~a original-name ":"))
                                                  (exn-message e)
                                                  (regexp-replace-quote (~a name ":")))
                                  (exn-continuation-marks e)
                                  (exn:fail:syntax-exprs e))))])
    (local-expand stx (syntax-local-context) '())))

(require (only-in racket/contract ->)
         (only-in syntax/parse syntax-parse id))
