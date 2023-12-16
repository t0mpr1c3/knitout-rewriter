#lang typed/racket

(provide (all-defined-out))

(require threading)
(require "fnitout-command.rkt"
         "fnitout-config.rkt"
         "fnitout-state.rkt"
         "fnitout-rule.rkt"
         "fnitout-pass.rkt"
         "fnitout-swap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Validation rules for Brother Lace without ribber
;;
;; NoSplit      - No equivalent of Split operation
;; SingleBed    - No ribber, so no knits on back bed, and no transfers to/from back bed except with using Lace Carriage
;; SingleColor  - Only one yarn carrier in use at any time
;; LaceCarriage - Racking can only take the values -1, 0, +1

(struct CarriagePass
  ([state    : CarriageState]
   [ops      : (Listof Operation)]
   [carriage : Carriage] ;; which carriage is being used for this Pass
   [dir      : (Option Dir)]
   [type     : PassType])
  #:transparent)

(struct CarriageState
  ([carriage-side : (HashTable Dir (Listof Carriage))] ;; '- means carriages starting on Left, '+ means carriages starting on Right
   [machine       : MachineState])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: schedule-passes : MachineConfig (Listof Pass) -> (Listof CarriagePass))
(define (schedule-passes config passes)
  (check-passes passes)
  (let* ([state0 (initial-carriage-state config passes)])
    null ;; placeholder
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; do basic checks on suitability of pattern
(: check-passes : (Listof Pass) -> Void)
(define (check-passes passes)
  (let ([types : (Listof PassType) (map Pass-type passes)])
    ;; only knit, move, and drop passes are allowed - not xfer passes
    ;; FIXME eventually we will want to accommodate Xfers that are not MOVEs
    (when (ormap (λ ([t : PassType]) (eq? 'xfer t))
                 types)
      (error 'check-passes "xfer passes are not permitted"))
    ;; pattern must have at least one knit pass
    (when (not (ormap (λ ([t : PassType]) (eq? 'knit t))
                      types))
      (error 'check-passes "pattern must contain at least one knit pass"))
    ;; passes must have direction assigned
    (when (ormap (compose false? Pass-dir)
                 ;(λ ([p : Pass]) (false? (Pass-dir p)))
                 passes)
      (error 'check-passes "pattern must have direction assigned"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns initial CarriageState
(: initial-carriage-state : MachineConfig (Listof Pass) -> CarriageState)
(define (initial-carriage-state config passes)
  (when (not (config-has-carriage? config 'Knit))
    (error 'initial-carriage-state "configuration must include Knit carriage")) ;; FIXME eventually we will want to allow Garter carriage use

  ;; initial side of Knit carriage is determined by direction of first knit pass
  (let ([dir0
         (for/or ([pass (in-list passes)]) : (Option Dir)
           (let ([dir (Pass-dir pass)])
             (if (eq? 'Knit (Pass-type pass)) dir #f)))])    
    (assert (Dir? dir0))

    ;; make initial state
    (let* ([state0 (CarriageState
                    (make-hasheq)
                    (make-state (MachineConfig-needle-count config)))]
           [carriage-side0 (CarriageState-carriage-side state0)]
           [p0    (first passes)]
           [type0 (Pass-type p0)])

      ;; set initial sides for carriages
      (if (config-has-carriage? config 'Lace)
          ;; Lace carriage always starts on the Left
          ;; Knit carriage can start on either side
          (if (eq? '- dir0)
              (if (eq? 'move type0)
                  (hash-set! carriage-side0 '- '(Lace Knit))  ;; Lace carriage will go first
                  (hash-set! carriage-side0 '- '(Knit Lace))) ;; Knit carriage will go first
              (begin
                (hash-set! carriage-side0 '- '(Lace))
                (hash-set! carriage-side0 '+ '(Knit))))
          (hash-set! carriage-side0 '+ '(Knit)))

      ;; return CarriageState
      state0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; end

;#|
(define config (MachineConfig 61 10 '(Knit Lace)))
(define state (make-state 61))

(require "k2f.rkt")
(define script (k2f "../knitout-examples/lace.knitout"))
(~>> script
     (script-passes state)
     passes-split-xfer
     passes-split-drop
     passes-assign-dir
     (filter (λ ([p : Pass]) (eq? 'xfer (Pass-type p))))
     passes-xfer->move
     passes-sort-out
     passes-sort-in
     passes-sort-by-needle-index ;; sort xfer/move/drop passes
     )
;|#