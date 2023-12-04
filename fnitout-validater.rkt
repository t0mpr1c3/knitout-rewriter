#lang typed/racket

;; https://doi.org/10.1145/3592449

(require racket/syntax
         syntax/parse
         syntax/warn
         threading)
(require/typed "fnitout-parser.rkt"
               [fnitout-parse (String -> Any)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Knitting machine state

S = (r, L ,Y ,A) consists of:

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
  ([rack : Integer] ;; racking offset
   [loops : (Listof Integer)] ;; bed.index -> loop count
   [carriers : (Listof Positive-Integer)] ;; physical yarn carrier positions (needle, direction)
   [attachments : (Listof Integer)])) ;; logical last-loop positions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; validate formal knitout AST
(: fnitout-validate : Any -> Boolean)
(define (fnitout-validate fk-stx)
  #t)

;; end