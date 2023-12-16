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
(: make-state : Positive-Integer -> MachineState)
(define (make-state needle-count)
  (MachineState
   0
   (make-hasheq
    (list (cons 'f ((inst make-vector Natural) needle-count 0))
          (cons 'b ((inst make-vector Natural) needle-count 0))))
   (make-hasheq)
   (make-hasheq)))

(: state-copy : MachineState -> MachineState)
(define (state-copy self)
  (let ([loops (MachineState-loops self)])
    (MachineState
     (MachineState-racking self)
     (make-hasheq
      (list (cons 'f (vector-copy (hash-ref loops 'f)))
            (cons 'b (vector-copy (hash-ref loops 'b)))))
     (hash-copy (MachineState-carrier-positions self))
     (hash-copy (MachineState-attachments self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getter functions

;; returns physical position of needle
(: needle-physical-position : Integer Needle -> Integer)
(define (needle-physical-position racking needle)
  (let ([bed (Needle-bed needle)]
        [idx (Needle-index needle)])
    (+ idx
       (if (eq? 'f bed)
           0
           racking))))

;; returns physical position of yarn carrier
(: carrier-physical-position (->* (Integer Needle) (Dir) Integer))
(define (carrier-physical-position racking needle [dir '-])
  (+ (needle-physical-position racking needle)
     (if (eq? '+ dir)
         1
         0)))

;; returns number of loops at needle location
(: get-loops : MachineState Needle -> Natural)
(define (get-loops state needle)
  (let* ([loops (MachineState-loops state)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-ref vec idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setter functions

;; sets number of loops at needle location
(: set-loops! : MachineState Needle Natural -> Void)
(define (set-loops! state needle val)
  (let* ([loops (MachineState-loops state)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-set! vec idx val)))

;; track newly created loops
(: update-loops! : MachineState Command -> Void)
(define (update-loops! state cmd)
  (let ([needle (op-needle cmd)]
        [yarns  (command-carriers cmd)])
    (set-loops! state needle (length yarns))))

;; moves loop count from source needle to target
(: move-loops! : MachineState Command -> Void)
(define (move-loops! state cmd)
  (let ([needle (op-needle cmd)]
        [target (op-target cmd)]
        [yarns  (command-carriers cmd)])
    ;; move loop count
    (set-loops! state target (+ (get-loops state target)
                                  (get-loops state needle)))
    ;; track newly created loops
    (update-loops! state cmd)))

;; sets physical position of yarn carriers
(: set-carrier-positions! : MachineState Command -> Void)
(define (set-carrier-positions! state cmd)
  (let* ([carrier-positions (MachineState-carrier-positions state)]
         [dir (op-dir cmd)]
         [needle (op-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)]
         [racking (MachineState-racking state)])
    (for ([y (in-list yarns)])
      (hash-set! carrier-positions y (carrier-physical-position racking needle dir)))))

;; sets attachments at needle specified by instruction
;; NB. JL also sets direction of attachment, but it is not used
(: set-attachments! : MachineState Command -> Void)
(define (set-attachments! state cmd)
  (let* ([attachments (MachineState-attachments state)]
         [needle (op-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)])
    (for ([y (in-list yarns)])
      (hash-set! attachments y needle))))

;; moves all attached loops from source needle to target
(: move-attachments! : MachineState Operation -> Void)
(define (move-attachments! state op)
  (let ([attachments (MachineState-attachments state)]
        [needle (op-needle op)]
        [target (op-target op)])
    (for ([y (in-hash-keys attachments)])
      (when (equal? needle
                    (hash-ref attachments y))
        (hash-set! attachments y target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run script, updating the machine state
(: run-script! : MachineState Script -> Void)
(define (run-script! state script)
  (for ([instr (in-list script)])
    (operate! state (Instruction-op instr))))

;; do operation, updating the machine state
(: operate! : MachineState Operation -> Void)
(define (operate! state op)
  (let ([cmds (op->cmds op)])
    (for ([cmd (in-list cmds)])
      (do-command! state cmd))))

;; updates machine state with first `n` operations from pos
;; returns list of operations with first `n` dropped
(: update! : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (update! machine ops n)
  (if (zero? n)
      ops
      (begin
        (operate! machine (car ops))
        (update! machine
                 (cdr ops)
                 (sub1 n)))))

;; do command, updating the machine state
(: do-command! : MachineState Command -> Void)
(define (do-command! state cmd)
  (cond 
    [(Tuck? cmd)
     (let ([needle (Tuck-needle cmd)])
       ;; increment loop count
       (set-loops! state needle (add1 (get-loops state needle))))
     (set-attachments! state cmd)
     (set-carrier-positions! state cmd)]

    [(Knit? cmd)
     (update-loops! state cmd)
     (set-attachments! state cmd)
     (set-carrier-positions! state cmd)]

    [(Split? cmd)
     (move-loops! state cmd)
     (move-attachments! state cmd)
     (set-attachments! state cmd)
     (set-carrier-positions! state cmd)]

    [(or (Miss? cmd)
         (In? cmd))
     (set-carrier-positions! state cmd)]

    [(Out? cmd)
     (let ([carrier-positions (MachineState-carrier-positions state)]
           [attachments       (MachineState-attachments       state)]
           [c                 (Carrier-val (Out-carrier cmd))])
       (hash-remove! carrier-positions c)
       (hash-remove! attachments       c))]

    [(Drop? cmd)
     (update-loops! state cmd)]

    [(Xfer? cmd)
     (move-loops! state cmd)
     (move-attachments! state cmd)]

    [(Rack? cmd)
     (set-MachineState-racking! state (Rack-racking cmd))]))

;; end