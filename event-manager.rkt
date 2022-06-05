#lang racket
(require "obfusca.rkt"
         sfml)

(provide (all-defined-out))

(def-class event-manager%
  object%
  (super-new)
  (field [events (make-hash)])

  (def/pub (on sym proc)
    (if (and (symbol? sym)
             (lambda? proc))
        (begin
          (hash-set! sym proc)
          #t)
        #f))

  (def/pub (dispatch evt)
    (case (event-type evt)
      ('sfEvtClosed
       (when (hash-has-key? events 'closed)
         ((hash@ events 'closed) evt)))

      ('sfEvtKeyPressed
       (when (hash-has-key? events 'key-pressed)
         ((hash@ events 'key-pressed) evt)))

      ('sfEvtMouseButtonPressed
       (when (hash-has-key? events 'mouse-button-pressed)
         ((hash@ events 'mouse-button-pressed) evt)))

      ('sfEvtMouseMove
       (when (hash-has-key? events 'mouse-move)
         ((hash@ events 'mouse-move) evt)))
    
      ('sfEvtMouseWheelScroll
       (when (hash-has-key? events 'mouse-wheel-scroll)
         ((hash@ events 'mouse-wheel-scroll) evt)))

      ('sfEvtTextEntered
       (when (hash-has-key? events 'text-entered)
         ((hash@ events 'text-entered) evt))))))

(def (event-manager? object-id)
  (is-a? object-id event-manager%))