#lang typed/racket

(provide (all-defined-out))

(require threading)
(require "fnitout-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; holds machine configuration
(struct MachineConfig
  ([needle-count  : Positive-Integer]
   [carrier-count : Positive-Integer]
   [beds          : (Listof Bed)]
   [carriages     : (Listof Carriage)]))

;; access function
(: config-has-carriage? : MachineConfig Carriage -> Boolean)
(define (config-has-carriage? self carriage)
  (let ([carriages (MachineConfig-carriages self)])
    (not (false? (memq carriage carriages)))))

;; end