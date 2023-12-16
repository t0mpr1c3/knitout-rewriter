#lang typed/racket

(provide (all-defined-out))

(require threading)
(require "fnitout-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Carriage
  (U 'Knit
     'Lace
     ;'Garter ;; FIXME not yet implemented
     ))
(define-predicate Carriage? Carriage)

;; holds machine configuration
(struct MachineConfig
  ([needle-count  : Positive-Integer]
   [carrier-count : Positive-Integer]
   [carriages     : (Listof Carriage)]))

;; access function
(: config-has-carriage? : MachineConfig Carriage -> Boolean)
(define (config-has-carriage? self carriage)
  (let ([carriages (MachineConfig-carriages self)])
    (not (false? (memq carriage carriages)))))

;; end