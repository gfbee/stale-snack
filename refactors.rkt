#lang racket

#| Collect refactorings, with eventual goal of summarizing to help programmer, and machine,
    to recognize and perform. |#

#;{(define (Pat p)
     (match p
       [`(TuplePat: ,pat ... "Boxed") `(list . ,(map Pat pat))]
       [`(VarPatVarName ,name) (symbol name)]
       [`(WildPat:) '_]
       [_ p]))
   (define Pat (λ-match/identity [`(TuplePat: ,pat ... "Boxed") `(list . ,(map Pat pat))]
                                 [`(VarPatVarName ,name) (symbol name)]
                                 [`(WildPat:) '_]))}

#;{{(define (GRHSHsApp a) (match a
                            [`(,(or 'GRHSHsApp: 'HsApp:) ,f ,e) `(,(GRHSHsApp f) ,(GRHSHsApp e))]
                            [`(HsVarVarName ,name) (symbol name)]
                            [_ a]))
    (define (GRHSs a) (match a [`(,a ... "EmptyLocalBinds") a]
                        [_ a]))
    (define (FunRhs a) (match a [`(FunRhs: ,a ,(or "Prefix" "Infix") "NoSrcStrict") `(FunRhs: ,a)]
                         [_ a]))}
   {(define GRHSHsApp
      (λ-match/identity [`(,(or 'GRHSHsApp: 'HsApp:) ,f ,e) `(,(GRHSHsApp f) ,(GRHSHsApp e))]
                        [`(HsVarVarName ,name) (symbol name)]))
    (define GRHSs (λ-match/identity [`(,a ... "EmptyLocalBinds") a]))
    (define FunRhs (λ-match/identity [`(FunRhs: ,a ,(or "Prefix" "Infix") "NoSrcStrict")
                                      `(FunRhs: ,a)]))}}

#;{(define (inline t) (if (list? t)
                          (match t
                            [`(,tag ,(? list? t′)) `(,tag . ,(inline t′))]
                            [_ (map inline t)])
                          t))
   (define inline (λ-match/identity [`(,tag ,(? list? t′)) `(,tag . ,(inline t′))]
                                    [`(,t ...) (map inline t)]))}

#;{(define (find key/s t)
     (match t
       ['() '()]
       [`(,h . ,t) (if (or (and (list? key/s) (member h key/s))
                           (≡ h key/s))
                       (list t)
                       (append-map (curry find key/s) t))]
       [else (list)]))
   (define (find key/s t)
     (match t
       [`(,h . ,t) (if (or (and (list? key/s) (member h key/s))
                           (≡ h key/s))
                       (list t)
                       (append-map (curry find key/s) t))]
       [else (list)]))}
