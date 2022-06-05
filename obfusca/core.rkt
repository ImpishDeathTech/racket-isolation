#lang racket
(provide (all-defined-out))

(define-syntax def-values
  (syntax-rules ()
    [(_ stx ...)
     (define-values stx ...)]))

(define ++ add1)
(define -- sub1)

(define (~ x)
  (or (not x)
      (null? x)))

(define-syntax &&
  (syntax-rules ()
    [(_ stx ...)
     (and stx ...)]))

(define-syntax ??
  (syntax-rules ()
    [(_ stx ...)
     (or stx ...)]))

(define (neq? x y) (not (eq? x y)))
(define (neqv? x y) (not (eqv? x y)))

(define hash@ hash-ref)
(define hs@ hash-ref)

(define list@ list-ref)
(define ls@ list-ref)
(define ls+ append)
(define len length)
(define ls->vec list->vector)

(define vector@ vector-ref)
(define vec@ vector-ref)

(define vector+= vector-append)
(define vec+ vector-append)
(define vec- vector-take)
(define vec-= vector-drop)
(define vec-=right vector-drop-right)
(define vec-len vector-length)
(define vec->ls vector->list)

(define string+= string-append)
(define string@ string-ref)
(define str+ string-append)
(define str@ string-ref)
(define str-len string-length)
(define str-trm string-trim)
(define str-div string-split)
(define str->ls string->list)

(define-syntax $
  (syntax-rules ()
    [(_ id-expr object-id)
     (get-field id-expr object-id)]))

(define-syntax $!
  (syntax-rules ()
    [(_ id-expr object-id expr)
     (set-field! id-expr object-id expr)]))

(define-syntax @
  (syntax-rules ()
    [(_ object-id stx ...)
     (send object-id stx ...)]))

(define-syntax @*
  (syntax-rules ()
    [(_ object-id stx ...)
     (send* object-id stx ...)]))

(define-syntax @+
  (syntax-rules ()
    [(_ object-id stx ...)
    (send+ object-id stx ...)]))

(define-syntax def
  (syntax-rules ()
    [(_ stx ...)
     (define stx ...)]))

(define-syntax def-class*
  (syntax-rules ()
    [(_ name-clause super-clause inheritance-clause stx ...)
     (define name-clause
       (class* super-clause
         inheritance-clause
         stx ...))]))

(define-syntax def-class
  (syntax-rules ()
    [(_ name-clause super-clause stx ...)
     (define name-clause
       (class super-clause
         stx ...))]))

(define-syntax def/pub
  (syntax-rules ()
    [(_ stx ...)
     (define/public stx ...)]))

(define-syntax def/priv
  (syntax-rules ()
    [(_ stx ...)
     (define/private stx ...)]))
    
(define lambda? procedure?)
(define λ? procedure?)

(define-syntax sigma
  (syntax-rules ()
    [(_ ([name start] test increment) body ...)
     (let ([name start])
        (let loop ()
          (when test
            body ...
            (set! name (+ name increment))
            (loop))))]
    
    [(_ [name clause] proc)
     (cond [(and (list? clause)
                 (procedure? proc))
            (let ([name clause])
              (let loop ()
                (unless (null? name)
                  (proc (car name))
                  (set! name (cdr name))
                  (loop))))]

           [(and (vector? clause)
                 (procedure? proc))
            (let ([name clause]
                  [limit (vector-length clause)]
                  [counter 0])
              (let loop ()
                (when (< counter limit)
                  (proc (vector-ref name counter))
                  (set! counter (add1 counter))
                  (loop))))]
           
           [(and (hash? clause)
                 (procedure? proc))
            (let ([name clause]
                  [keys (hash-keys clause)])
              (let loop ()
                (unless (null? keys)
                  (proc (car keys) (hash-ref name (car keys)))
                  (set! keys (cdr keys))
                  (loop))))]
           [else (printf "\n\nError! Ill formed sigma!\nExpected: iterable and procedure with 1-2 arguments\ngot: ~a, ~a\n\n" clause proc)])]
    
    [(_ test body ...)
     (let loop ()
       (when test
         body ...
         (loop)))]))

(define-syntax Σ
  (syntax-rules ()
    [(_ ([name start] test increment) body ...)
     (let ([name start])
        (let loop ()
          (when test
            body ...
            (set! name (+ name increment))
            (loop))))]
    
    [(_ [name clause] proc)
     (cond [(and (list? clause)
                 (procedure? proc))
            (let ([name clause])
              (let loop ()
                (unless (null? name)
                  (proc (car name))
                  (set! name (cdr name))
                  (loop))))]

           [(and (vector? clause)
                 (procedure? proc))
            (let ([name clause]
                  [limit (vector-length clause)]
                  [counter 0])
              (let loop ()
                (when (< counter limit)
                  (proc (vector-ref name counter))
                  (set! counter (add1 counter))
                  (loop))))]
           
           [(and (hash? clause)
                 (procedure? proc))
            (let ([name clause]
                  [keys (hash-keys clause)])
              (let loop ()
                (unless (null? keys)
                  (proc (car keys) (hash-ref name (car keys)))
                  (set! keys (cdr keys))
                  (loop))))]
           [else (printf "\n\nError! Ill formed sigma!\nExpected: iterable and procedure with 1-2 arguments\ngot: ~a, ~a\n\n" clause proc)])]
    
    [(_ test body ...)
     (let loop ()
       (when test
         body ...
         (loop)))]))
