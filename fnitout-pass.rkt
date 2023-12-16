#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (all-defined-out))

(require threading)
(require "fnitout-command.rkt"
         "fnitout-state.rkt"
         "fnitout-swap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pass type
(define-type PassType
  (U 'knit  ;; knit/tuck operations, etc.
     'drop  ;; Drop operations: FIXME Drop is just Knit with no yarn, so 'drop passes can be renamed 'knit
     'xfer  ;; xfers without changes in racking
     'move));; xfers plus changes in racking

;; Pass struct
(struct Pass
  ([state : MachineState]       ;; machine state after all previous passes have been run
   [ops   : (Listof Operation)]
   [dir   : (Option Dir)]
   [type  : PassType])
  #:transparent)

(: pass-dir : (Listof Operation) -> (Option Dir))
(define (pass-dir p-ops)
  (let loop : (Option Dir)
    ([ops p-ops])
    (if (null? ops)
        #f
        (let ([op (first ops)])
          (if (op-dir? op)
              (op-dir op)
              (loop (cdr ops)))))))

(: pass-type : (Listof Operation) -> PassType)
(define (pass-type p-ops)
  (let loop : PassType
    ([ops p-ops])
    (if (null? ops)
        (if (ormap Drop? p-ops)
            'drop
            'xfer)
        (let ([op (first ops)])
          (if (op-dir? op)
              'knit
              (loop (cdr ops)))))))

(: pass-split-at : Pass (Option Index) -> (Listof Pass))
(define (pass-split-at self idx)
  (let ([ops (Pass-ops self)])
    (if (false? idx)
        (list self)
        (let* ([ms0   (Pass-state self)]
               [ops0  (take ops idx)]
               [dir0  (pass-dir  ops0)]
               [type0 (pass-type ops0)]
               [pass0 (Pass ms0
                            ops0
                            dir0
                            type0)]
               [ms1   (state-copy ms0)]          
               [ops1  (drop ops idx)]
               [dir1  (pass-dir  ops1)]
               [type1 (pass-type ops1)]
               [pass1 (Pass ms1
                            ops1
                            dir1
                            type1)])
          (for ([op (in-list ops0)])
            (operate! ms1 op))
          (list pass0 pass1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run pass, updating the machine state
(: run-pass! : MachineState Pass -> Void)
(define (run-pass! state pass)
  (for ([op (in-list (Pass-ops pass))])
    (operate! state op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Chunk the Script into Passes.
;; Assumes script is valid.
(: script-passes : MachineState Script -> (Listof Pass))
(define (script-passes p-state self)
  (let* ([state0 (state-copy p-state)] ;; copy initial state of machine
         [ops0 (map (λ ([x : Instruction])
                      (Instruction-op x))
                    self)]
         [dir0  (pass-dir  ops0)]
         [type0 (pass-type ops0)])
    (let loop : (Listof Pass)
      ([state : MachineState       (state-copy state0)]
       [ops   : (Listof Operation) ops0]
       [dir   : (Option Dir)       dir0]
       [type  : PassType           type0]
       [aops  : (Listof Operation) null]
       [a     : (Listof Pass)      null])
      (if (null? ops)
          (reverse
           (cons (Pass state
                       (reverse aops)
                       dir
                       type)
                 a))
          (let ([op (first ops)])
            (if (and (op-dir? op)
                     (not (eq? (op-dir op)
                               dir)))
                ;; new Pass
                (let ([state1 (state-copy state)]
                      [ops1 (reverse aops)])
                  (for ([op (in-list ops1)])
                    (operate! state1 op))
                  (loop state1
                        (cdr ops)
                        (op-dir op)
                        'knit
                        (list op)
                        (cons (Pass state
                                    ops1
                                    dir
                                    type)
                              a)))
                ;; continue
                (loop state
                      (cdr ops)
                      dir
                      type
                      (cons op aops)
                      a)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assigns direction to transfer passes
(: passes-assign-dir : (Listof Pass) -> (Listof Pass))
(define (passes-assign-dir passes)
  (let* ([dirs0 (map (λ ([p : Pass]) (Pass-dir p))
                     passes)]
         [dirs1 (assign-dir-aux dirs0 #f #t null)]
         [dirs2 (assign-dir-aux dirs1 #f #t null)]) ;; repeat to catch initial #f's
    (for/list ([p : Pass         (in-list passes)]
               [d : (Option Dir) (in-list dirs2 )]) : (Listof Pass)
      (assert (Dir? d))
      (struct-copy Pass p
                   [dir d]))))

(: assign-dir-aux : (Listof (Option Dir)) (Option Dir) Boolean (Listof (Option Dir)) ->  (Listof (Option Dir)))
(define (assign-dir-aux ds d0 ok? a)
  (if (null? ds)
      (if ok?
          a ;; reversed
          (assign-dir-aux (reverse a) #f #t null))
      (let ([d (car ds)])
        (if (false? d)
            (let ([d1 (cond [(false? d0) #f]
                            [(eq? '+ d0) '-]
                            [else        '+])])
              (if (false? d0)
                  (assign-dir-aux (cdr ds)
                                  d1
                                  ok?
                                  (cons d1 a))
                  (assign-dir-aux (cdr ds)
                                  d1
                                  #f
                                  (cons d1 a))))
            (assign-dir-aux (cdr ds)
                            d
                            ok?
                            (cons d a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterate over Passes
(: foreach-pass : (Listof Pass) (-> Pass (Listof Pass)) -> (Listof Pass))
(define (foreach-pass passes split-func)
  (apply append
         (for/list ([p : Pass (in-list passes)]) : (Listof (Listof Pass))
           (split-func p))))

;; splits Pass into one or more Passes
(: pass-split : Pass (-> Pass Pass) (-> Pass (Listof Pass)) -> (Listof Pass))
(define (pass-split pass sort-func split-func)
  (let loop : (Listof Pass)
    ([p0 : Pass          pass]
     [a  : (Listof Pass) null])
    (let* ([p1  (sort-func p0)]
           [ps  (split-func p1)]
           [ps1 (first ps)]
           [a1  (cons ps1 a)])
      (if (= 1 (length ps))
          ;; not split
          (reverse a1)
          ;; split
          (loop (second ps)
                a1)))))

;; sorts Operations in Pass
(: pass-sort : Pass (-> Operation Boolean) -> Pass)
(define (pass-sort pass predicate)
  (let ([state0 (state-copy (Pass-state pass))])
    (let loop : Pass
      ([ops : (Listof Operation) (Pass-ops pass)]
       [a   : (Listof Operation) null])
      (if (or (null? ops)
              (null? (cdr ops)))
          (struct-copy Pass pass
                       [ops (reverse (append ops a))])
          (let ([op1 (first  ops)]
                [op2 (second ops)])
            (if (and (predicate op1)
                     (swap-possible? state0 op1 op2))
                ;; swap
                (begin
                  (operate! state0 op2)
                  (loop (cons op1 (cddr ops))
                        (cons op2 a)))
                ;; don't swap
                (begin
                  (operate! state0 op1)
                  (loop (cdr ops)
                        (cons op1 a)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting Xfers to the end of each pass
;; then splitting them into a separate transfer pass
(: passes-split-xfer : (Listof Pass) -> (Listof Pass))
(define (passes-split-xfer passes)
  (foreach-pass passes pass-split-xfer))

;; sorts Xfers to the end of the pass
;; then splits them into a separate transfer pass
;; and updates machine state
(: pass-split-xfer : Pass -> (Listof Pass))
(define (pass-split-xfer self)
  (pass-split self sort-xfer split-xfer))

;; split Xfers into separate pass
(: split-xfer : Pass -> (Listof Pass))
(define (split-xfer pass)
  (let* ([ops  (Pass-ops pass)]
         [idx0 (index-where ops (λ ([op : Operation])
                                  (or (Xfer? op)
                                      (MOVE? op)
                                      (Rack? op)
                                      (RACK? op))))])
    (if (false? idx0)
        (list pass)
        (let* ([idx1 (if (zero? idx0)
                         (index-where ops (λ ([op : Operation])
                                            (and (not (Xfer? op))
                                                 (not (MOVE? op))
                                                 (not (Rack? op))
                                                 (not (RACK? op)))))
                         idx0)])
          (pass-split-at pass idx1)))))

;; * Given initial machine state, attempt to sort operations into 'knitting passes'
;;   (ordered sequences of Knit/Tuck and Miss operations) and 'transfer passes'
;;   (sequences of Xfer and Rack operations).
;; * Knitting passes go first, so iterate through operations looking for Xfers
;;   then move Xfers as far to the back as they will go.
(: sort-xfer : Pass -> Pass)
(define (sort-xfer pass)
  (let ([state0 (state-copy (Pass-state pass))] ;; copy initial state of machine
        [ops0 (Pass-ops pass)])
    (let loop : Pass
      ([ops : (Listof Operation) ops0]
       [a   : (Listof Operation) null])
      (if (or (null? ops)
              (null? (cdr ops)))
          (struct-copy Pass pass
                       [ops (reverse (append ops a))])
          (let ([op1 (first  ops)]
                [op2 (second ops)])
            (if (and (or (Xfer? op1)
                         (MOVE? op1)) ;; ignore SHIFTs which are hard to move
                     (not (or (Xfer? op2)
                              (MOVE? op2)
                              (Rack? op2)
                              (RACK? op2))) ;; don't swap Xfers with other Xfers, or Rack operations
                     (swap-possible? state0 op1 op2))
                ;; swap
                (begin
                  (operate! state0 op2)
                  (loop (cons op1 (cddr ops))
                        (cons op2 a)))
                (if (and (or (Xfer? op1)
                             (MOVE? op1)
                             (Rack? op1)
                             (RACK? op1))
                         (or (Xfer? op2)
                             (MOVE? op2)
                             (Rack? op2)
                             (RACK? op2)))
                    (let-values ([(head tail) (sort-xfer-aux state0 ops)])
                      (let ([op3 (first head)])
                        (if (or (Xfer? op3)
                                (MOVE? op3)
                                (Rack? op3)
                                (RACK? op3))
                            ;; move on
                            (loop tail
                                  (append (reverse head) a))
                            ;; recurse
                            (loop (append head tail)
                                  a))))
                    ;; don't swap
                    (begin
                      (operate! state0 op1)
                      (loop (cdr ops)
                            (cons op1 a))))))))))

;; Move forward to the next operation that is not an Xfer/Rack
;; and try to sort it to the front of the entire sequence of Xfers.
;; FIXME This assumes that the operations in the Xfer/Rack
;; sequence do not affect machine state in a way that affects
;; the calculation of extents: that assumption may not hold.
(: sort-xfer-aux : MachineState (Listof Operation) -> (values (Listof Operation) (Listof Operation)))
(define (sort-xfer-aux p-state p-ops)
  (let* ([idx0 (index-where p-ops (λ ([op : Operation])
                                    (and (not (Xfer? op))
                                         (not (MOVE? op))
                                         (not (Rack? op))
                                         (not (RACK? op)))))])
    (if (false? idx0)
        (values p-ops null)
        (let* ([ops0 (reverse (take p-ops (add1 idx0)))] ;; reverse sequence from 0 .. idx0
               [ops1
                (let loop : (Listof Operation)
                  ([ops : (Listof Operation) ops0]
                   [a   : (Listof Operation) null])
                  (if (or (null? ops)
                          (null? (cdr ops)))
                      (append ops a)
                      (let ([op1 (first  ops)]
                            [op2 (second ops)])
                        (if (swap-possible? p-state op1 op2)
                            ;; swap
                            (loop (cons op1 (cddr ops))
                                  (cons op2 a))
                            ;; end
                            (append a (reverse ops))))))])
          (values ops1
                  (drop p-ops (add1 idx0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting Drops to the end of each pass
;; then splitting them into a separate knit pass
(: passes-split-drop : (Listof Pass) -> (Listof Pass))
(define (passes-split-drop passes)
  (foreach-pass passes pass-split-drop))

;; sorts Drops to the end of the pass
;; then splits them into a separate knit pass
(: pass-split-drop : Pass -> (Listof Pass))
(define (pass-split-drop pass)
  (pass-split pass sort-drop split-drop))

;; split Drops into separate pass
(: split-drop : Pass -> (Listof Pass))
(define (split-drop pass)
  (let* ([ops  (Pass-ops pass)]
         [idx0 (index-where ops Drop?)])
    (if (false? idx0)
        (list pass)
        (let* ([idx1 (if (zero? idx0)
                         (index-where ops (compose not Drop?))
                         idx0)])
          (pass-split-at pass idx1)))))

;; sorts Drop operations to the end of the pass
(: sort-drop : Pass -> Pass)
(define (sort-drop pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (pass-sort pass Drop?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting operations by needle index
(: passes-sort-by-needle-index : (Listof Pass) -> (Listof Pass))
(define (passes-sort-by-needle-index passes)
  (for/list ([p : Pass (in-list passes)]) : (Listof Pass)
    (sort-by-needle-index p)))

;; FIXME the sort will fail if there are state-changing operations mixed in
;; e.g. Rack operations alongside Xfer, so some checks are required to
;; make sure that such operations are absent before sorting
(: sort-by-needle-index : Pass -> Pass)
(define (sort-by-needle-index pass)
  (if (eq? 'knit (Pass-type pass))
      pass
      ;; xref | move | drop
      (let* ([ops0 (Pass-ops pass)]
             [dir  (Pass-dir pass)]
             [r    (MachineState-racking (Pass-state pass))]
             [end  (if (eq? '+ dir) +999999 -999999)]
             [idxs0
              (for/list ([op (in-list ops0)]) : (Listof (Pairof Integer Operation))
                (cons (if (op-needle? op)
                          (let* ([n (op-needle op)]
                                 [b (Needle-bed n)]
                                 [x (Needle-index n)])
                            (- x
                               (if (eq? 'b b) r 0)))
                          end)
                      op))]
             [idxs1 ((inst sort (Pairof Integer Operation) Integer)
                     idxs0 #:key car
                     (if (eq? '+ dir) < >))]
             [ops1  ((inst map Operation (Pairof Integer Operation))
                     cdr idxs1)])
        (struct-copy Pass pass
                     [ops ops1]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting Out operations to the end of each pass
(: passes-sort-out : (Listof Pass) -> (Listof Pass))
(define (passes-sort-out passes)
  (for/list ([p : Pass (in-list passes)]) : (Listof Pass)
    (sort-out p)))

;; sorts Out operations to the end of the pass
(: sort-out : Pass -> Pass)
(define (sort-out pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (pass-sort pass Out?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting In operations to the beginning of each pass
(: passes-sort-in : (Listof Pass) -> (Listof Pass))
(define (passes-sort-in passes)
  (for/list ([p : Pass (in-list passes)]) : (Listof Pass)
    (sort-in p)))

;; sorts In operations to the beginning of the pass
(: sort-in : Pass -> Pass)
(define (sort-in pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (let* ([state (Pass-state pass)]
             [ops0  (Pass-ops pass)]
             [ops1  (sort-in-aux state ops0 null #f)])
        (struct-copy Pass pass
                     [ops ops1]))))

(: sort-in-aux : MachineState (Listof Operation) (Listof Operation) (Option Natural) -> (Listof Operation))
(define (sort-in-aux state ops head idx)
  (let ([idx0 (index-where ops In?)])
    (if (or (false? idx0)
            (and (not (false? idx))
                 (not (< idx0 idx))))
        (append head ops)
        (if (zero? idx0)
            (sort-in-aux state
                         (cdr ops)
                         (append head (list (car ops)))
                         #f)
            (let* ([state0 (state-copy state)]
                   [ops0 (update! state0 ops (sub1 idx0))]
                   [op1 (first  ops0)]
                   [op2 (second ops0)])
              (if (swap-possible? state0 op1 op2)
                  ;; swap
                  (sort-in-aux state
                               (append (take ops (sub1 idx0))
                                       (list op2 op1)
                                       (drop ops (add1 idx0)))
                               head
                               idx0)
                  ;; don't swap
                  (sort-in-aux state
                               (drop ops (sub1 idx0))
                               (append head
                                       (take ops (sub1 idx0)))
                               idx0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: passes-xfer->move : (Listof Pass) -> (Listof Pass))
(define (passes-xfer->move passes)
  (for/list ([pass (in-list passes)]) : (Listof Pass)
    (xfer->move pass)))

;; turns transfer pass featuring blocks of Xfers separated by RACKs
;; into a collection of MOVE operations
;; FIXME this will fail if there are redundant pairs of Xfers mixed in,
;; e.g.  xfer f0 b0 / xfer b0 f0, so these pairs need to be merged out
(: xfer->move : Pass -> Pass)
(define (xfer->move pass)
  (if (not (eq? 'xfer (Pass-type pass)))
      pass
      (let* ([state0   (state-copy (Pass-state pass))] ;; copy initial machine state
             [racking0 (MachineState-racking state0)]
             [ops0     (Pass-ops pass)]
             [state1   (state-copy state0)]
             [xfers
              (let loop : (Listof (Pairof Xfer Integer))
                ([ops ops0]
                 [a   : (Listof (Pairof Xfer Integer)) null])
                (if (null? ops)
                    (reverse a)
                    (let ([op (car ops)])
                      (operate! state1 (car ops))
                      (if (Xfer? op)
                          (loop (cdr ops)
                                (cons (cons op (MachineState-racking state1)) a))                          
                          (loop (cdr ops)
                                a)))))]
             ;[dummy (void (for ([x (in-list xfers)]) (println x)))]
             [moves
              (let loop : (Listof Operation)
                ([lst xfers]
                 [a   : (Listof Operation) null])
                (if (null? lst)
                    (reverse a)
                    (let* ([pair1 (car lst)]
                           [xfer1 (car pair1)]
                           [r1 (cdr pair1)]
                           [target1 (op-target xfer1)]
                           [i (index-where (cdr lst)
                                           (λ ([x : (Pairof Xfer Integer)])
                                             (equal? target1 (op-needle (car x)))))])
                      (if (false? i)
                          (error 'pass-xfer->move "failed: no match for ~a" xfer1) ;; FIXME allow unmatched Xfers
                          (let* ([pair2 (list-ref (cdr lst) i)]
                                 [xfer2 (car pair2)]
                                 [r2 (cdr pair2)]
                                 [n1 (op-needle xfer1)]
                                 [b1 (Needle-bed n1)]
                                 [x1 (Needle-index n1)]
                                 [n2 (op-target xfer2)]
                                 [b2 (Needle-bed n2)]
                                 [x2 (Needle-index n2)]
                                 [j (- x2 x1)]
                                 [move (MOVE n1 r1 j)])
                            ;; sanity checks
                            (assert (eq? b1 b2))
                            (assert (= (- r2 r1)
                                       (if (eq? 'f b1) j (- j))))
                            (loop (append (take (cdr lst) i)
                                          (drop (cdr lst) (add1 i)))
                                  (if (zero? j)
                                      ;; ignore redundant move
                                      a
                                      ;; otherwise keep move
                                      (cons move a))))))))])
        (struct-copy Pass pass
                     [ops  moves]
                     [type 'move]))))

;; end

#|
(define machine (make-MachineState 30))

(require "k2f.rkt")
(define script (k2f "../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.k"))
(~>> script
     (script-passes machine)
     passes-split-xfer
     passes-split-drop
     passes-assign-dir
     ;(passes-xfer->move machine)
     passes-sort-out
     passes-sort-in
     passes-sort-by-needle-index
     )
|#
