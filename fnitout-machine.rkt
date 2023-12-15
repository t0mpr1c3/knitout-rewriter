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

(: machine-copy : MachineState -> MachineState)
(define (machine-copy machine)
  (let ([loops (MachineState-loops machine)])
    (MachineState
     (MachineState-racking machine)
     (make-hasheq
      (list (cons 'f (vector-copy (hash-ref loops 'f)))
            (cons 'b (vector-copy (hash-ref loops 'b)))))
     (hash-copy (MachineState-carrier-positions machine))
     (hash-copy (MachineState-attachments machine)))))

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
(define (get-loops machine needle)
  (let* ([loops (MachineState-loops machine)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-ref vec idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setter functions

;; sets number of loops at needle location
(: set-loops! : MachineState Needle Natural -> Void)
(define (set-loops! machine needle val)
  (let* ([loops (MachineState-loops machine)]
         [bed (Needle-bed needle)]
         [idx (Needle-index needle)]
         [vec (hash-ref loops bed)])
    (vector-set! vec idx val)))

;; track newly created loops
(: update-loops! : MachineState Command -> Void)
(define (update-loops! machine cmd)
  (let ([needle (op-needle cmd)]
        [yarns  (command-carriers cmd)])
    (set-loops! machine needle (length yarns))))

;; moves loop count from source needle to target
(: move-loops! : MachineState Command -> Void)
(define (move-loops! machine cmd)
  (let ([needle (op-needle cmd)]
        [target (op-target cmd)]
        [yarns  (command-carriers cmd)])
    ;; move loop count
    (set-loops! machine target (+ (get-loops machine target)
                                  (get-loops machine needle)))
    ;; track newly created loops
    (update-loops! machine cmd)))

;; sets physical position of yarn carriers
(: set-carrier-positions! : MachineState Command -> Void)
(define (set-carrier-positions! machine cmd)
  (let* ([carrier-positions (MachineState-carrier-positions machine)]
         [dir (op-dir cmd)]
         [needle (op-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)]
         [racking (MachineState-racking machine)])
    (for ([y (in-list yarns)])
      (hash-set! carrier-positions y (carrier-physical-position racking needle dir)))))

;; sets attachments at needle specified by instruction
;; NB. JL also sets direction of attachment, but it is not used
(: set-attachments! : MachineState Command -> Void)
(define (set-attachments! machine cmd)
  (let* ([attachments (MachineState-attachments machine)]
         [needle (op-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)])
    (for ([y (in-list yarns)])
      (hash-set! attachments y needle))))

;; moves all attached loops from source needle to target
(: move-attachments! : MachineState Operation -> Void)
(define (move-attachments! machine op)
  (let ([attachments (MachineState-attachments machine)]
        [needle (op-needle op)]
        [target (op-target op)])
    (for ([y (in-hash-keys attachments)])
      (when (equal? needle
                    (hash-ref attachments y))
        (hash-set! attachments y target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run script, updating the machine state
(: run-script! : MachineState Script -> Void)
(define (run-script! machine script)
  (for ([instr (in-list script)])
    (operate! machine (Instruction-op instr))))

;; run pass, updating the machine state
(: run-pass! : MachineState Pass -> Void)
(define (run-pass! machine pass)
  (for ([op (in-list (Pass-ops pass))])
    (operate! machine op)))

;; do operation, updating the machine state
(: operate! : MachineState Operation -> Void)
(define (operate! machine op)
  (let ([cmds (op->cmds op)])
    (for ([cmd (in-list cmds)])
      (do-command! machine cmd))))

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
(define (do-command! machine cmd)
  (cond 
    [(Tuck? cmd)
     (let ([needle (Tuck-needle cmd)])
       ;; increment loop count
       (set-loops! machine needle (add1 (get-loops machine needle))))
     (set-attachments! machine cmd)
     (set-carrier-positions! machine cmd)]

    [(Knit? cmd)
     (update-loops! machine cmd)
     (set-attachments! machine cmd)
     (set-carrier-positions! machine cmd)]

    [(Split? cmd)
     (move-loops! machine cmd)
     (move-attachments! machine cmd)
     (set-attachments! machine cmd)
     (set-carrier-positions! machine cmd)]

    [(or (Miss? cmd)
         (In? cmd))
     (set-carrier-positions! machine cmd)]

    [(Out? cmd)
     (let ([carrier-positions (MachineState-carrier-positions machine)]
           [attachments       (MachineState-attachments       machine)]
           [c                 (Carrier-val (Out-carrier cmd))])
       (hash-remove! carrier-positions c)
       (hash-remove! attachments       c))]

    [(Drop? cmd)
     (update-loops! machine cmd)]

    [(Xfer? cmd)
     (move-loops! machine cmd)
     (move-attachments! machine cmd)]

    [(Rack? cmd)
     (set-MachineState-racking! machine (Rack-racking cmd))]))

;; end