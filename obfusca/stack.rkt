#lang racket
(require "core.rkt")

(provide stack<%>
         make-stack
         stack?
         (prefix-out stack: (combine-out last-in-first-out%
                                         first-in-first-out%
                                         any-in-any-out%
                                         last-in-first-out?
                                         first-in-first-out?
                                         any-in-any-out?
                                         lifo?
                                         fifo?
                                         aiao?)))

(def stack<%>
  (interface () size push pop top clear empty?))

(def (stack? object-id)
  (is-a? object-id stack<%>))

(def-class* last-in-first-out%
  object%
  (stack<%>)
  (super-new)
  (field [stack (list)])

  (def/pub (size) (length stack))
  
  (def/pub (push data)
    (set! stack (cons data stack)))

  (def/pub (pop)
    (define out (car stack))
    (set! stack (cdr stack))
    out)

  (def/pub (top) (car stack))

  (def/pub (clear)
    (set! stack (list))
    (collect-garbage))

  (def/pub (empty?)
    (null? stack)))

(def (last-in-first-out? object-id)
  (is-a? object-id last-in-first-out%))

(def lifo? last-in-first-out?)

(def-class* first-in-first-out%
  object%
  (stack<%>)
  (super-new)
  (field [stack (vector)])

  (def/pub (size) (vector-length stack))

  (def/pub (push data)
    (set! stack (vector+= (vector data) stack)))
  
  (def/pub (pop)
    (define out (vector-ref stack (- (vector-length stack) 1)))
    (set! stack (vector-drop-right stack 1))
    out)

  (def/pub (top) (vector-ref stack (- vector-length stack) 1))

  (def/pub (empty?) (vector-empty? stack))

  (def/pub (clear)
    (set! stack (vector))
    (collect-garbage)))

(def (first-in-first-out? object-id)
  (is-a? object-id first-in-first-out%))

(def fifo? first-in-first-out?)

(def-class* any-in-any-out%
  object%
  (stack<%>)
  (super-new)
  (field [stack (vector)]
         [current-top 'head])

  (def/pub (size) (vector-length stack))

  (def/pub (push data sym)
    (if (symbol? sym)
        (cond [(or (eq? sym 'head)
                   (eq? sym 'beg))
               (set! stack (vector-append (vector data) stack))
               (set! current-top 'head)
               #t]
              [(or (eq? sym 'tail)
                   (eq? sym 'end))
               (set! stack (vector-append stack (vector data)))
               (set! current-top 'tail)
               #t]
              [else #f])
        #f))
  
  (def/pub (pop sym)
    (if (symbol? sym)
        (cond [(or (eq? sym 'head)
                   (eq? sym 'beg))
               (set! stack (vector-drop stack 1))
               (set! current-top 'head)
               #t]
              [(or (eq? sym 'tail)
                   (eq? sym 'end))
               (set! stack (vector-drop-right stack 1))
               (set! current-top 'tail)
               #t]
              [else #f])
        #f))

  (def/pub (top) current-top)

  (def/pub (head) (vector-ref stack 0))

  (def/pub (tail) (vector-ref stack (- (vector-length stack) 1)))

  (def/pub (empty?) (vector-empty? stack))

  (def/pub (clear)
    (set! stack (vector))
    (collect-garbage)))

(def (any-in-any-out? object-id)
  (is-a? object-id any-in-any-out%))

(def aiao? any-in-any-out?)

(define-syntax make-stack
  (syntax-rules ()
    [(_ sym)
     (if (symbol? sym)
         (cond [(eq? sym 'lifo)
                (new last-in-first-out%)]
               [(eq? sym 'fifo)
                (new first-in-first-out%)]
               [(eq? sym 'aiao)
                (new any-in-any-out%)]
            [else null])
         null)]
    [(_)
     (new last-in-first-out%)]))