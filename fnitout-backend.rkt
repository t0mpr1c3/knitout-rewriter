#lang typed/racket

(provide (all-defined-out))

(require typed/racket/draw
         threading)
(require "fnitout-command.rkt"
         "fnitout-config.rkt"
         "fnitout-state.rkt"
         "fnitout-rule.rkt"
         "fnitout-pass.rkt"
         "fnitout-swap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Casting on
;;
;; No instructions are provided for casting on. As far as the pattern goes, 
;; it is assumed that some kind of casting on procedure has already happened.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lace knitting on Brother machines:
;;
;; The Lace carriage (LC) only does transfers and does no knitting.
;; The LC always starts on the Left hand side of the machine.
;; Lace extension rails must be used to accommodate both carriages.
;;
;; When machine knitting, you are always looking at the Purl side of the fabric
;; so transfer directions are reversed compared to the RS.
;;
;; R transfers are performed on the first pass (L to R)
;; L transfers are performed on the second pass (R to L)
;;
;; Consecutive transfers require separate transfers for each stitch in the sequence.
;; The LC must transfer the stitch furthest away from the eyelets first.
;;
;; Note that electronic patterns differ from punchcard patterns:
;; https://alessandrina.com/2018/04/13/revisiting-knitting-with-2-carriages-single-bed-910-vs-ayab-so-far/

;; When combining Lace with Tuck stitches:
;; *  Set the cam buttons on the KC to Tuck.
;; *  Move rubber wheels on sinker plate into working position.
;; *  Stitches to be Knit are selected, stitches to be Tucked are deselected.
;; *  KC starts on RHS to preselect for first row of Tuck.

;; Machine knitters using Japanese model machines are familiar with some tuck rules
;; ie. white pixels/unpunched squares that form tuck loops should have a black pixel/
;; punched hole on each side of them and not occur vertically for more than 4 rows
;; in order for stitches to knit off and form properly.
;;
;; https://alessandrina.com/2023/06/28/passap-to-brother-6-exploring-a-possible-tuck-stitch-design/

;; Similarly, when combining Lace with Slip stitches (aka Skip stitches, Miss):
;; *  Set the cam buttons on the KC to Part.
;; *  Stitches to be Knit are selected, stitches to be Slipped are deselected.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Validation rules for Brother Lace without ribber:
;;
;; SingleBed    - No ribber, so no knits on back bed, and no transfers to/from
;                 back bed except with using Lace Carriage
;; SingleColor  - Only one yarn carrier in use at any time
;; KnitCarriage - No equivalent of Split operation
;;              - Knit stitches can be combined with either Slip or Tuck stitches,
;;                both not both in the same pass
;; LaceCarriage - Racking can only take the values -1, 0, +1

(struct CarriagePass
  ([row      : Positive-Integer] ;; row number, starting at 1
   [pass     : Positive-Integer] ;; pass number, starting at 1
   [carriage : Carriage] ;; which carriage is being used for this Pass
   [dir      : (Option Dir)]
   [type     : PassType]
   [state    : CarriageState]   
   [ops      : OpList])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates schedule of carriage passes.
;; FIXME fix caston sequence
;; FIXME fix selection for first row
(: make-carriage-passes : MachineConfig (Listof Pass) -> (Listof CarriagePass))
(define (make-carriage-passes config passes)
  (let ([ps0 (process-passes passes)])
    (passes-check ps0)
    (let* ([state (initial-carriage-state config ps0)]
           [beds  (MachineConfig-beds config)]
           [cps0
            (let loop : (Listof CarriagePass)
              ([ps ps0]
               [rn : Positive-Integer 1]
               [pn : Positive-Integer 1]
               [a  : (Listof CarriagePass) null])
              (if (null? ps)
                  a
                  (let* ([p0    (car ps)]
                         [type0 (Pass-type p0)])
                    (cond [(or (eq? 'stst type0)
                               (eq? 'tuck type0)
                               (eq? 'part type0)
                               (eq? 'drop type0))
                           (let* ([cp0 (knit-carriage-passes! state p0 rn pn)]
                                  [n   (length cp0)])
                             (loop (cdr ps)
                                   (+ 1 rn)
                                   (+ n pn)
                                   (append a cp0)))]
                          [(eq? 'move type0)
                           (let* ([cp0 (lace-carriage-passes! state p0 rn pn)]
                                  [n   (length cp0)])
                             (loop (cdr ps)
                                   rn
                                   (+ n pn)
                                   (append a cp0)))]
                          [(eq? 'caston type0)
                           (loop (cdr ps)
                                 rn
                                 pn
                                 a)] ;; omit
                          [(eq? 'xfer type0)
                           (error 'schedule-passes "xfer pass not permitted")]
                          [else
                           (error 'schedule-passes "~a pass not recognized" type0)]))))])
      (for ([cp0 (in-list cps0)])
        (let* ([cs0  (CarriagePass-state cp0)]
               [sel0 (pass-selection config cp0)])
          (update-selection! cs0 beds sel0)))
      cps0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Does some initial data processing on passes.
(: process-passes : (Listof Pass) -> (Listof Pass))
(define (process-passes passes)
  (~> passes
      passes-rm-nop
      passes-split-xfer
      passes-split-drop
      passes-move
      passes-sort-out
      passes-sort-in
      passes-sort-by-needle-index
      passes-caston
      passes-tuck
      passes-part
      passes-stst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the selection necessary to complete the operations in a pass.
(: pass-selection : MachineConfig CarriagePass -> Selection)
(define (pass-selection config pass)
  (let* ([needle-count (MachineConfig-needle-count config)]
         [beds (MachineConfig-beds config)]
         [state (CarriagePass-state pass)]
         [selection : Selection (make-hasheq)]
         [type (CarriagePass-type pass)]
         [ops (CarriagePass-ops pass)])
    ;; set default selection to unselected (0)
    (for ([b (in-list beds)])
      (hash-set! selection b (make-vector needle-count 0)))
    ;; set final selection based on operations in pass
    (for ([op (in-list ops)])
      (when (op-needle? op)
        (let* ([n (op-needle op)]
               [b (Needle-bed n)]
               [x (Needle-index n)]
               [v (hash-ref selection b)]
               [s (cond [(Knit? op) (if (eq? 'stst type) 0 1)]
                        [(Miss? op) 0] ;; assuming 'part pass
                        [(Tuck? op) 0] ;; assuming 'tuck pass
                        [(MOVE? op) 1] ;; assuming 'lace pass
                        [else       0])])
          (vector-set! v x s))))
    selection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makes Knit carriage passes and updates state.
(: knit-carriage-passes! : CarriageState Pass Positive-Integer Positive-Integer -> (Listof CarriagePass))
(define (knit-carriage-passes! state pass rn pn)
  (let* ([cs   (CarriageState-carriage-side state)]
         [dir  (Pass-dir pass)]
         [dir0 (if (false? dir)
                   (opposite (knit-carriage-side cs))
                   dir)]
         [dir1  (opposite dir0)]
         [c1    (hash-ref cs dir1)])
    (if (and (pair? c1)
             (eq? 'Knit (car c1)))
        ;; Knit carriage is on the correct side
        (list (knit-pass! state pass dir0 rn pn))
        ;; insert empty Knit pass to get the carriage to the other side
        (list (empty-knit-pass! state dir0 rn pn)
              (knit-pass! state pass dir0 rn (add1 pn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makes Knit carriage pass and updates state.
(: knit-pass! : CarriageState Pass Dir Positive-Integer Positive-Integer -> CarriagePass)
(define (knit-pass! state0 pass0 dir0 rn pn)
  (let* ([cs0    (CarriageState-carriage-side state0)]
         [sel0   (CarriageState-selection state0)]
         [ms0    (Pass-state pass0)]
         [ops0   (Pass-ops   pass0)]
         [type0  (Pass-type  pass0)]         
         [dir1   (opposite dir0)]
         [c0     (hash-ref cs0 dir0)]
         [c1     (hash-ref cs0 dir1)]
         [cs1    (hash-copy cs0)]
         [sel1   (hash-copy sel0)]
         [ms1    (MachineState-copy ms0)]
         [state1 (CarriageState cs1 sel1 ms1)])
    (assert (and (not (false? c1))
                 (not (null?  c1))
                 (eq? 'Knit (car c1))))
    (hash-set! cs0 dir1 (cdr c1))
    (hash-set! cs0 dir0 (cons 'Knit c0))
    (CarriagePass rn
                  pn
                  'Knit
                  dir1
                  type0
                  state1
                  ops0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makes empty Knit pass and updates state.
(: empty-knit-pass! : CarriageState Dir Positive-Integer Positive-Integer -> CarriagePass)
(define (empty-knit-pass! state0 dir0 rn pn)
  (let* ([ms0    (CarriageState-machine state0)]
         [cs0    (CarriageState-carriage-side state0)]
         [sel0   (CarriageState-selection state0)]
         [dir1   (opposite dir0)]
         [c0     (hash-ref cs0 dir0)]
         [c1     (hash-ref cs0 dir1)]
         [cs1    (hash-copy cs0)]
         [sel1   (hash-copy sel0)]
         [ms1    (MachineState-copy ms0)]
         [state1 (CarriageState cs1 sel1 ms1)])
    (assert (and (not (false? c0))
                 (not (null?  c0))
                 (eq? 'Knit (car c0))))
    (hash-set! cs0 dir0 (cdr c0))
    (hash-set! cs0 dir1 (cons 'Knit c1))
    (CarriagePass rn
                  pn
                  'Knit
                  dir1
                  'empty
                  state1
                  null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makes Lace carriage passes and updates state.
(: lace-carriage-passes! : CarriageState Pass Positive-Integer Positive-Integer -> (Listof CarriagePass))
(define (lace-carriage-passes! state pass rn pn)
  ;; Lace carriage must start on the Left (-)
  (let* ([cs (CarriageState-carriage-side state)]
         [c- (hash-ref cs '-)]
         [i  (index-of c- 'Lace)])
    ;; Lace carriage must be the first or second carriage on the Left
    (assert (and (not (false? i))
                 (<= i 1)))
    (if (zero? i)
        ;; Knit carriage is next on Left
        (lace-carriage-passes-aux1! state pass rn pn)
        ;; Lace carriage is next after the Knit carriage
        (append (list (empty-knit-pass! state '+ rn pn))
                (lace-carriage-passes-aux1! state pass rn (add1 pn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Separates consecutive MOVE operations into sequences of LC passes.
(: lace-carriage-passes-aux1! : CarriageState Pass Positive-Integer Positive-Integer -> (Listof CarriagePass))
(define (lace-carriage-passes-aux1! state pass rn pn)
  (let* ([ops (Pass-ops pass)]
         [left-moves  (filter (λ ([op : Operation])
                                (and (MOVE? op)
                                     (negative? (MOVE-j op))))
                              ops)]
         [right-moves (filter (λ ([op : Operation])
                                (and (MOVE? op)
                                     (positive? (MOVE-j op))))
                              ops)]
         [left-sorted  ((inst sort Operation)
                        #:key (compose Needle-index op-needle)
                        left-moves >)]
         [right-sorted ((inst sort Operation)
                        #:key (compose Needle-index op-needle)
                        right-moves <)])
    (let loop : (Listof CarriagePass)
      ([lm  left-sorted]
       [rm  right-sorted]
       [p   : Positive-Integer pn]
       [a   : (Listof CarriagePass) null])
      (if (and (null? lm)
               (null? rm))
          a
          (let-values ([(lm0 lm1) (filter-consecutive -1 lm)]
                       [(rm0 rm1) (filter-consecutive  1 rm)])
            (loop lm1
                  rm1
                  (+ 2 pn)
                  (append a (lace-carriage-passes-aux2! state pass lm0 rm0 rn p))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Filters consecutive Lace transfers.
(: filter-consecutive : Integer OpList -> (values OpList OpList))
(define (filter-consecutive lag lst)
  (if (null? lst)
      (values null null)
      (let loop ([xs : OpList    (cdr lst)]
                 [x0 : Operation (car lst)]
                 [a  : OpList     null]
                 [a~ : OpList     null])
        (if (null? xs)
            (values (reverse (cons x0 a))
                    (reverse a~))
            (let ([x1 (car xs)])
              (if (= (Needle-index (op-needle x1))
                     (+ (Needle-index (op-needle x0)) lag))
                  (loop (cdr xs)
                        x1
                        a
                        (cons x0 a~))
                  (loop (cdr xs)
                        x1
                        (cons x0 a)
                        a~)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a pair of LC passes, selecting for L then R transfers, and updates state.
(: lace-carriage-passes-aux2! : CarriageState Pass OpList OpList Positive-Integer Positive-Integer -> (Listof CarriagePass))
(define (lace-carriage-passes-aux2! state pass0 left-moves right-moves rn pn)
  (let* ([cs0    (CarriageState-carriage-side state)]
         [sel0   (CarriageState-selection state)]
         [ms0    (Pass-state pass0)]
         [c-     (hash-ref cs0 '-)]
         [c+     (hash-ref cs0 '+)]
         [ms1    (MachineState-copy ms0)]
         [cs1    (hash-copy cs0)]
         [sel1   (hash-copy sel0)]
         [state1 (CarriageState cs1 sel1 ms1)]
         #|
         [right-selections
          (map move->select right-moves)]
         |#)
    ;; LC is the next carriage on the Left
    (assert (and (not (false? c-))
                 (not (null?  c-))
                 (eq? 'Lace (car c-))))
    ;; R transfers on first pass (L to R)
    (let* ([cp1 (CarriagePass rn
                              pn
                              'Lace
                              '+
                              'lace
                              state1
                              right-moves)])
      (run! ms0 right-moves)
      (hash-set! cs0 '- (cdr c-))
      (hash-set! cs0 '+ (cons 'Lace c+))
      ;; L transfers on second pass (R to L)
      (let* ([cs2    (hash-copy cs0)]
             [ms2    (MachineState-copy ms0)]
             [sel2   (hash-copy sel0)]
             [state2 (CarriageState cs2 sel2 ms2)]
             #|
             [left-selections
              (map move->select left-moves)]
             |#
             [cp2    (CarriagePass rn
                                   (add1 pn)
                                   'Lace
                                   '-
                                   'lace
                                   state2
                                   left-moves)])
        (run! ms0 left-moves)
        (hash-set! cs0 '- c-)
        (hash-set! cs0 '+ c+)
        (list cp1 cp2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do basic checks on suitability of pattern.
(: passes-check : (Listof Pass) -> Void)
(define (passes-check passes)
  (let ([types : (Listof PassType) (map Pass-type passes)])
    ;; only knit, move, and drop passes are allowed - not xfer passes
    ;; FIXME eventually we will want to accommodate Xfers that are not MOVEs
    (when (ormap (λ ([t : PassType]) (eq? 'xfer t))
                 types)
      (error 'check-passes "xfer passes are not permitted"))
    ;; pattern must have at least one knit pass
    (when (not (ormap (λ ([t : PassType])
                        (or (eq? 'stst t)
                            (eq? 'tuck t)
                            (eq? 'part t)))
                      types))
      (error 'check-passes "pattern must contain at least one knit pass"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns initial CarriageState.
(: initial-carriage-state : MachineConfig (Listof Pass) -> CarriageState)
(define (initial-carriage-state config passes)
  (when (not (config-has-carriage? config 'Knit))
    (error 'initial-carriage-state "configuration must include Knit carriage")) ;; FIXME eventually we will want to allow Garter carriage use

  ;; initial side of Knit carriage is determined by direction of first knit pass
  (let ([dir0
         (for/or ([pass (in-list passes)]) : (Option Dir)
           (let ([dir  (Pass-dir pass)]
                 [type (Pass-type pass)])
             (if (or (eq? 'stst type)
                     (eq? 'tuck type)
                     (eq? 'part type)) dir #f)))])
    (assert (Dir? dir0))

    ;; make initial state
    (let* ([state0 (CarriageState
                    (make-hasheq)
                    (make-hasheq)
                    (make-MachineState (MachineConfig-needle-count config)))]
           [carriage-side0 (CarriageState-carriage-side state0)]
           [p0     (first passes)])

      ;; set initial sides for carriages
      (hash-set! carriage-side0 '- null)
      (hash-set! carriage-side0 '+ null)
      (if (config-has-carriage? config 'Lace)
          ;; Lace carriage always starts on the Left
          (if (eq? '+ dir0)
              (hash-set! carriage-side0 '- '(Knit Lace)) ;; Knit carriage goes first
              (begin
                (hash-set! carriage-side0 '- '(Lace))
                (hash-set! carriage-side0 '+ '(Knit))))
          ;; Knit carriage can start on either side
          (hash-set! carriage-side0 (opposite dir0) '(Knit)))

      ;; return CarriageState
      state0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; exports data for AYAB as PNG file
(: export-carriage-passes : (Listof CarriagePass) Path-String -> Void)
(define (export-carriage-passes cps filename)
  (let ([bitmap (carriage-passes->bitmap cps)])
    (send bitmap save-file filename 'png) ;; returns Boolean
    (void)))

;; exports CarriagePass data as Bitmap% object
(: carriage-passes->bitmap : (Listof CarriagePass) -> (Instance Bitmap%))
(define (carriage-passes->bitmap cps)
  (let* ([data : (Listof (Vectorof Integer))
               (map (λ ([cp : CarriagePass])
                      (~> cp
                          CarriagePass-state
                          CarriageState-selection
                          (hash-ref 'f)))
                    cps)]
         [h (length data)]
         [w (vector-length (car data))]
         [b (apply bytes-append
                   (map selection->bytes
                        data))])
    (make-object bitmap% b w h))) ;; placeholder

(: selection->bytes : (Vectorof Integer) -> Bytes)
(define (selection->bytes v0)
  (let* ([w0 (vector-length v0)]
         [w1 (* 8 (ceiling (/ w0 8)))] ;; padded
         [v1 (vector-append v0 (make-vector (- w1 w0) 0))]) ;; padded with 0s
    (list->bytes
     (for/list ([i : Natural (in-range 0 w1 8)]) : (Listof Byte)
       (let* ([v (~> v1
                     (vector-drop i)
                     (vector-take 8))]
              [b (+ (* #x01 (vector-ref v 0))
                    (* #x02 (vector-ref v 1))
                    (* #x04 (vector-ref v 2))
                    (* #x08 (vector-ref v 3))
                    (* #x10 (vector-ref v 4))
                    (* #x20 (vector-ref v 5))
                    (* #x40 (vector-ref v 6))
                    (* #x80 (vector-ref v 7)))])
         (assert (byte? b))
         b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; end

#|
(define config (MachineConfig 200 10 '(f) '(Knit Lace)))
(define state (make-MachineState 200))

(require "k2f.rkt")
(define script (k2f "../knitout-examples/lace.knitout"))
(~>> script
     (script-passes state)
     ;(take _ 13) ;; output too long
     (make-carriage-passes config)
     (export-carriage-passes _ "lace.png"))
|#