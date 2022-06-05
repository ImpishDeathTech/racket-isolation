#lang racket
(require "obfusca.rkt"
         "event-manager.rkt")

(provide (all-defined-out))

(def state<%>
  (interface () on-init on-update on-draw on-kill on-freeze on-thaw get-events))

(def (state? object-id)
  (is-a? object-id state<%>))

(define-syntax def-state
  (syntax-rules ()
    [(_ name-id stx ...)
     (def-class* name-id
       object%
       (state<%>)
       (super-new)
       (field [events (new event-manager%)])
       stx ...)]))

(def-class* state-machine%
  object%
  (object-manager<%>)
  (super-new)
  (field [stack (make-stack)]
         [new-state #f]
         [is-push? #f]
         [is-pop? #f]
         [is-rep? #f])

  (def/pub (size) (@ stack size))

  (def/pub (push new-state-id is-replacing?)
    (if (and (state? new-state-id)
             (boolean? is-replacing?))
        (begin
          (set! is-push? #t)
          (set! is-rep? is-replacing?)
          (set! new-state new-state-id)
          #t)
        #f))

  (def/pub (pop) (set! is-pop? #t))

  (def/pub (top) (@ stack top))

  (def/pub (at index) (@ stack at index))

  (def/pub (empty?) (@ stack empty?))

  (def/pub (clear) (@ stack clear))

  (def/pub (kill-all)
    (sigma (~ (@ stack empty?))
           (@ (@ stack top) on-kill)
           (@ stack pop))
    (@ this clear))

  (def/pub (process)
    (when (&& is-pop?
              (~ (@ stack empty?)))
      (@ (@ stack top) on-kill)
      (@ stack pop)

      (unless (@ stack empty?)
        (@ (@ stack top) on-thaw))

      (set! is-pop? #f))

    (when is-push?
      (unless (@ stack empty?)
        (if is-rep?
            (begin
              (@ (@ stack top) on-kill)
              (@ stack pop)
              (set! is-rep? #f))
            (@ (@ stack top) on-freeze)))

      (@ stack push new-state)
      (@ (@ stack top) on-init)
      (set! is-push? #f))))

(def (state-machine? object-id)
  (is-a? object-id state-machine%))