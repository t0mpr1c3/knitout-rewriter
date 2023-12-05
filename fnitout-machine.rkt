#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (all-defined-out))

(require "fnitout-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Knitting machine state

S = (r, L, Y, A) consists of:

• r ∈ Z, the racking offset, or the offset of the needles on the
back bed relative to the front bed. At offset r , back needle
b.x - r is across from front needle f.x.

• L ∈ nLoc → N, a partial function with default value 0 that
reports the number of loops on each needle.

• Y ∈ N → Z, a partial function that gives the current physical
position of the yarn carriers. If the value is ⊥ (the default
value), then we say that the carrier is inactive.

• A ∈ N → ycLoc a partial function that gives the logical
carrier location of where each yarn carrier is attached to a
loop. An inactive carrier (with value ⊥) is not attached.

We define the empty state as S∅ = (0, [], [], []).
|#

(struct MachineState
  ([racking : Integer] ;; racking offset
   [loops : (HashTable Bed (Vectorof Natural))] ;; bed[index] -> loop count
   [carrier-positions : (HashTable Positive-Integer Integer)] ;; physical yarn carrier positions
   [attachments : (HashTable Positive-Integer Needle)]) ;; logical last-loop positions
  #:mutable
  #:transparent)

;; constructor
(: make-MachineState : Positive-Integer -> MachineState)
(define (make-MachineState needle-count)
  (MachineState
   0
   (make-hasheq
    (list (cons 'f ((inst make-vector Natural) needle-count 0))
          (cons 'b ((inst make-vector Natural) needle-count 0))))
   (make-hasheq)
   (make-hasheq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-loops : MachineState Needle -> Natural)
(define (get-loops self needle)
  (let* ([loops (MachineState-loops self)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-ref vec idx)))

(: set-loops! : MachineState Needle Natural -> Void)
(define (set-loops! self needle val)
  (let* ([loops (MachineState-loops self)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-set! vec idx val)))

;; end