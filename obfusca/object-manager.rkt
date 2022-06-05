#lang racket

(require "core.rkt"
         "stack.rkt")

(provide (all-defined-out))

(def object-manager<%>
  (interface (stack<%>) at kill-all))

(def (object-manager? object-id)
  (is-a? object-id object-manager<%>))

(def-class* object-manager%
  object%
  (object-manager<%>)
  (super-new)
  (field [stack (list)])

  (def/pub (size) (len stack))

  (def/pub (push data)
    (if (object? data)
        (begin
          (set! stack (cons data stack))
          #t)
        #f))

  (def/pub (pop)
    (def out (car stack))
    (set! stack (cdr stack))
    out)

  (def/pub (top) (car stack))

  (def/pub (clear)
    (set! stack (list))
    (collect-garbage))

  (def/pub (at index)
    (list@ stack index))
  
  (def/pub (empty?) (null? stack))

  (def/pub (kill-all)
    (sigma [ls stack]
           (λ (value)
             (cond [(method-in-interface? 'kill (object-interface value))
                    (@ value kill)]
                   [(method-in-interface? 'destroy (object-interface value))
                    (@ value destroy)])))
    (@ this clear)))