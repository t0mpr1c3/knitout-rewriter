#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (struct-out Validator)
         make-validator
         validate!)

(require threading)
(require "fnitout-command.rkt"
         "fnitout-state.rkt"
         "fnitout-config.rkt"
         "fnitout-rule.rkt"
         "fnitout-pass.rkt")

(define current-validity : (Parameterof (Listof String))
  (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME needle offset
(struct Validator
  ([config : MachineConfig]  ;; number of needles in each bed
   [state  : MachineState]   ;; knitting machine state (mutable)
   [rules  : (Listof Rule)]) ;; optional validation rules
  #:transparent)

;; constructor
(: make-validator (->* (MachineConfig) ((U Rule (Listof Rule))) Validator))
(define (make-validator config [rules null])
  (Validator
   config
   (make-state (MachineConfig-needle-count config))  
   (if (list? rules) rules (list rules))))

;; access function
(: validator-has-rule? : Validator Rule -> Boolean)
(define (validator-has-rule? self rule)
  (let ([rules (Validator-rules self)])
    (not (false? (memq rule rules)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; validates formal knitout script, updating machine state
(: validate! : Validator Script -> (Listof Record))
(define (validate! self script)
  (let ([state (Validator-state self)]
        [expanded (script-expand script)])
    (let vloop ([cmds : (Listof (Pairof Command String)) expanded]
                [acc  : (Listof Record) null])
      (if (null? cmds)
          (reverse acc)
          (let ([cmd     (caar cmds)]
                [comment (cdar cmds)])
            (current-validity null)
            (validate-cmd self cmd)
            (operate! state cmd)
            (vloop (cdr cmds)
                   (cons (Record cmd
                                 comment
                                 (current-validity))
                         acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generates errors as side effect
(: validate-cmd : Validator Command -> Void)
(define (validate-cmd self cmd)
  (let ([state (Validator-state self)])
    (when (and (validator-has-rule? self 'NoSplit)
               (Split? cmd))
      (invalid "Split operation is not permitted"))
    
    (when (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd)
              (Miss? cmd)
              (Out? cmd))
      (check-carrier-positions state cmd))
  
    (when (command-carriers? cmd)
      (check-carriers self cmd))
  
    (when (op-needle? cmd)
      (check-needle self cmd))
  
    (when (op-target? cmd)
      (check-target self cmd))
  
    (cond
      [(In? cmd)
       (let ([carrier-positions (MachineState-carrier-positions state)]
             [c                 (Carrier-val (In-carrier cmd))])
         (when (hash-has-key? carrier-positions c)
           (invalid "yarn carrier is already in"))
         (when (and (validator-has-rule? self 'SingleColor)
                    (not (zero? (length (hash-keys carrier-positions)))))
           (invalid "in Single Color mode, only one yarn carrier can be in use at any time")))]
    
      [(Drop? cmd)
       (let ([needle (Drop-needle cmd)])
         (when (zero? (get-loops state needle))
           (invalid "needle has no loops to drop")))]
    
      [(Rack? cmd)
       (let ([old (MachineState-racking state)]
             [new (Rack-racking cmd)])
         (when (= old new)
           (invalid "redundant Rack instruction"))
         (unless (= 1 (abs (- old new)))
           (invalid "Rack instruction can only change the racking by +/-1"))
         (when (and (validator-has-rule? self 'LaceCarriage)
                    (> (abs new) 1))
           (invalid "when using the Lace Carriage, racking may only take the values -1, 0, or 1")))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; checks that all yarn carriers are at physical position corresponding to [n.x, dir]_r
(: check-carrier-positions : MachineState Command -> Void)
(define (check-carrier-positions state cmd)
  (let* ([direction (command-carrier-position-dir cmd)] ;; NB command `Out` is a special case
         [needle    (op-needle cmd)]
         [carriers  (command-carriers cmd)]
         [racking   (MachineState-racking state)]
         [expected  (carrier-physical-position racking needle direction)]
         [positions (MachineState-carrier-positions state)])
    (for ([c (in-list carriers)])
      (let ([y (Carrier-val c)])
        (if (not (hash-has-key? positions y))
            (invalid (format "yarn carrier ~a is not in action" y))
            (let ([actual (hash-ref positions y)])
              (unless (= expected actual)
                (invalid (format "expected yarn carrier ~a at position ~a, but it is at ~a" y expected actual)))))))))

;; checks that the number of yarn carriers allowed by the machine is not exceeded
(: check-carriers : Validator Command -> Void)
(define (check-carriers validator cmd)
  (let* ([config (Validator-config validator)]
         [carrier-count (MachineConfig-carrier-count config)]
         [carriers (command-carriers cmd)])
    (for ([c (in-list carriers)])
      (let ([n (Carrier-val c)])
        (when (> n carrier-count)
          (invalid (format "carrier number ~a exceeds carrier count ~a" n carrier-count)))))))

;; checks that source and target needles are aligned
(: check-needle : Validator Command -> Void)
(define (check-needle validator cmd)
  (let* ([config (Validator-config validator)]
         [needle-count (MachineConfig-needle-count config)]
         [needle (op-needle cmd)]
         [bed    (Needle-bed   needle)]
         [idx    (Needle-index needle)])
    (unless (< idx needle-count)
      (invalid (format "needle index ~a is too big" idx)))
    (when (< idx 0)
      (invalid (format "needle index ~a is negative" idx)))
    (when (and (validator-has-rule? validator 'SingleBed)
               (eq? 'b bed)
               (not (and (validator-has-rule? validator 'LaceCarriage)
                         (Xfer? cmd))))
      (invalid "in Single Bed mode, needle used must be on front bed except for transfers using Lace Carriage"))))

;; checks that source and target needles are aligned
(: check-target : Validator Command -> Void)
(define (check-target validator cmd)
  (let* ([config  (Validator-config validator)]
         [needle-count (MachineConfig-needle-count config)]
         [rules   (Validator-rules validator)]
         [state   (Validator-state validator)]
         [racking (MachineState-racking state)]
         [needle  (op-needle cmd)]
         [target  (op-target cmd)]
         [bed     (Needle-bed   target)]
         [idx     (Needle-index target)])
    (when (eq? (Needle-bed needle)
               (Needle-bed target))
      (invalid "needle and target are on same bed"))
    (unless (= (carrier-physical-position racking needle)
               (carrier-physical-position racking target))
      (invalid "needle and target are not aligned"))
    (unless (< idx needle-count)
      (invalid (format "target needle index ~a is too big" idx)))
    (when (< idx 0)
      (invalid (format "target needle index ~a is negative" idx)))
    (when (and (validator-has-rule? validator 'SingleBed)
               (eq? 'b bed)
               (not (and (validator-has-rule? validator 'LaceCarriage)
                         (Xfer? cmd))))
      (invalid "in Single Bed mode, target needle must be on front bed except for transfers using Lace Carriage"))))

;; displays error and stores error message in `current-validity` parameter
(: invalid : String -> Void)
(define (invalid msg)
  (displayln (format "Error in formal knitout validation: ~a" msg))
  (current-validity
   (append
    (current-validity)
    (list msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; end

#|
(define config (MachineConfig 61 10 '(Knit Lace)))
(define validator (make-validator config '(NoSplit SingleBed SingleColor LaceCarriage)))

(require "k2f.rkt")
(define script (k2f "../knitout-examples/lace.knitout"))
(validate! validator script)
|#

#|
;; test equivalence of MOVE operations with commands in different order
(define caston4-ops : (Listof Operation)
  (list
   ;; cast on
   (In   (Direction '-)
         (Needle 'f 5)
         (Carrier 1))
   (Tuck (Direction '-)
         (Needle 'f 4)
         (Length 1.0)
         (Yarn (Carrier 1)
               (Length 1.0)))
   (Miss (Direction '-)
         (Needle 'f 3)
         (Carrier 1))
   (Tuck (Direction '-)
         (Needle 'f 2)
         (Length 1.0)
         (Yarn (Carrier 1)
               (Length 1.0)))
   (Miss (Direction '-)
         (Needle 'f 1)
         (Carrier 1))
   (Miss (Direction '-)
         (Needle 'f 0)
         (Carrier 1))
   (Miss (Direction '+)
         (Needle 'f 0)
         (Carrier 1))
   (Tuck (Direction '+)
         (Needle 'f 1)
         (Length 1.0)
         (Yarn (Carrier 1)
               (Length 1.0)))
   (Miss (Direction '+)
         (Needle 'f 2)
         (Carrier 1))
   (Tuck (Direction '+)
         (Needle 'f 3)
         (Length 1.0)
         (Yarn (Carrier 1)
               (Length 1.0)))
   (Miss (Direction '+)
         (Needle 'f 4)
         (Carrier 1))))
|#

#|
(define v1 (make-validator 10 1))
(define caston4-script (map (λ ([op : Operation])
                  (Instruction op ""))
                 caston4-ops))
(validate v1 caston4-script)
|#

#|
(define v1 (make-validator 10 1))
(define move1-ops
  (append
   caston4-ops
   (list
   (Miss (Direction '-)
         (Needle 'f 4)
         (Carrier 1))
   (SHIFT (Needle 'f 4)
          0
          -1)
   (RACK 1 -1)
   )))
(define move1-script (map (λ ([op : Operation])
                  (Instruction op ""))
                 move1-ops))
(validate v1 move1-script)
(println v1)

(define v2 (make-validator 10 1))
(define move2-ops
  (append
   caston4-ops
   (list
   (Miss (Direction '-)
         (Needle 'f 4)
         (Carrier 1))
   (RACK 0 1)
   (SHIFT (Needle 'f 4)
          1
          -1)
   )))
(define move2-script (map (λ ([op : Operation])
                  (Instruction op ""))
                 move2-ops))
(validate v2 move2-script)
(println v2)
|#