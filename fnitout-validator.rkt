#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (struct-out Record)
         (struct-out Validator)
         make-Validator
         validate
         needle-physical-position
         carrier-physical-position)

(require "fnitout-command.rkt"
         "fnitout-machine.rkt")

(define current-validity : (Parameterof (Listof String))
  (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; holds machine configuration
(struct Validator
  ([needle-count : Positive-Integer]  ;; number of needles in each bed
   [carrier-count : Positive-Integer] ;; number of yarn carriers
   [machine : MachineState]) ;; knitting machine state (mutable)
  #:transparent)

;; constructor
(: make-Validator : Positive-Integer Positive-Integer -> Validator)
(define (make-Validator needle-count carrier-count)
  (Validator
   needle-count  ;; FIXME use to create runtime contract on Needle
   carrier-count ;; FIXME use to create runtime contract on Carrier
   (make-MachineState needle-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; validates formal knitout script
(: validate : Validator Script -> (Listof Record))
(define (validate self script)
  (let ([machine (Validator-machine self)]
        [expanded (script-expand script)])
    (let vloop ([cmds : (Listof (Pairof Command String)) expanded]
                [acc  : (Listof Record) null])
      (if (null? cmds)
          (reverse acc)
          (let ([cmd     (caar cmds)]
                [comment (cdar cmds)])
            (current-validity null)
            (validate-cmd machine cmd)
            (operate! machine cmd)
            (vloop (cdr cmds)
                   (cons
                    (Record
                     cmd
                     comment
                     (current-validity))
                    acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generates errors as side effect
(: validate-cmd : MachineState Command -> Void)
(define (validate-cmd machine cmd)
  (when (or (Tuck? cmd)
            (Knit? cmd)
            (Split? cmd)
            (Miss? cmd)
            (Out? cmd))
    (check-carrier-positions machine cmd))
  
  (when (or (Split? cmd)
            (Xfer? cmd))
    (check-target machine cmd))
  
  (cond
    [(In? cmd)
     (let ([carrier-positions (MachineState-carrier-positions machine)]
           [c                 (Carrier-val (In-carrier cmd))])
       (when (hash-has-key? carrier-positions c)
         (invalid "yarn carrier is already in")))]
    
    [(Drop? cmd)
     (let ([needle (Drop-needle cmd)])
       (when (zero? (get-loops machine needle))
         (invalid "needle has no loops to drop")))]
    
     [(Rack? cmd)
      (let ([old (MachineState-racking machine)]
            [new (Rack-racking cmd)])
        (when (= old new)
          (invalid "redundant Rack instruction"))
        (unless (= 1 (abs (- old new)))
          (invalid "Rack instruction can only change the racking by +/-1")))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; checks that all yarn carriers are at physical position corresponding to [n.x, dir]_r
(: check-carrier-positions : MachineState Command -> Void)
(define (check-carrier-positions machine cmd)
  (let* ([direction (command-carrier-position-dir cmd)] ;; NB command `Out` is a special case
         [needle    (op-needle cmd)]
         [carriers  (command-carriers cmd)]
         [racking   (MachineState-racking machine)]
         [expected  (carrier-physical-position racking needle direction)]
         [positions (MachineState-carrier-positions machine)])
    (for ([c (in-list carriers)])
      (let ([y (Carrier-val c)])
        (if (not (hash-has-key? positions y))
            (invalid (format "yarn carrier ~a is not in action" y))
            (let ([actual (hash-ref positions y)])
              (unless (= expected actual)
                (invalid (format "expected yarn carrier ~a at position ~a, but it is at ~a" y expected actual)))))))))

;; checks that source and target needles are aligned
(: check-target : MachineState Command -> Void)
(define (check-target machine cmd)
  (let* ([needle  (op-needle cmd)]
         [target  (op-target cmd)]
         [racking (MachineState-racking machine)])
    (when (eq? (Needle-bed needle)
               (Needle-bed target))
      (invalid "needle and target are on same bed"))
    (unless (= (carrier-physical-position racking needle)
               (carrier-physical-position racking target))
      (invalid "needle and target are not aligned"))))

;; displays error and stores error message in `current-validity` parameter
(: invalid : String -> Void)
(define (invalid msg)
  (displayln (format "Error in formal knitout validation: ~a" msg))
  (current-validity
   (append
    (current-validity)
    (list msg))))

;; end
