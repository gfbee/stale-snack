#lang racket/base

(provide syntax-tooltip)

(define syntax/tooltip/c (syntax? syntax? string? . -> . syntax?))

(define (syntax-tooltip source-syntax stx message-string)
  (syntax-parse source-syntax [(head:id . _)
                               (syntax-property stx 'mouse-over-tooltips
                                                (vector source-syntax
                                                        (syntax-position source-syntax)
                                                        (sub1 (+ (syntax-position #'head)
                                                                 (syntax-span #'head)))
                                                        message-string))]))

(require (only-in racket/contract ->)
         (only-in syntax/parse syntax-parse id))
