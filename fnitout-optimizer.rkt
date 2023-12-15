#lang typed/racket

;; https://doi.org/10.1145/3592449

(provide swap-possible?
         swap
         merge-rack
         merge-miss
         squish
         slide
         conjugate
         assign-dir-aux
         sort-xfer
         sort-xfer-aux         
         sort-drop!
         passes-sort-out
         sort-out!
         passes-sort-in
         sort-in
         sort-in-aux)

(require threading)
(require "fnitout-machine.rkt"
         "fnitout-command.rkt"
         "fnitout-validator.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 1
;; knitout operations with disjoint extents can be reordered
(: swap : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (swap machine ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'swap "swap not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]         
         [ops0 (update! machine0 ops pos)]
         [op1  (first  ops0)] ;; (list-ref ops pos)
         [op2  (second ops0)] ;; (list-ref ops (add1 pos))
         [ext1 (extent machine0 op1)]
         [ext2 (extent machine0 op2)])
    (unless (extents-disjoint? ext1 ext2)
      (error 'swap "swap not possible at position ~a" pos))
    ;; do swap
    (append
     (take ops pos)
     (list op2 op1)
     (drop ops (+ 2 pos)))))

(: swap-possible? : MachineState Operation Operation -> Boolean)
(define (swap-possible? machine op1 op2)
  (extents-disjoint? (extent machine op1)
                     (extent machine op2)))

;; definition D.2
;; definition D.6 (MOVE)
(: extent : MachineState Operation -> (Option Extent))
(define (extent machine op)
  (cond [(Nop? op)
         #f]
        [(or (Rack? op)
             (RACK? op))
         (Extent (Interval -inf.0 +inf.0)
                 (Interval +inf.0 +inf.0))]
        [else
         (let* ([racking (MachineState-racking machine)]
                [needle (op-needle op)]
                [bed (Needle-bed needle)]
                [pos (needle-physical-position
                      racking
                      needle)])
           (cond [(Tuck? op)
                  (let* ([carriers (command-carriers op)]
                         [ys (map Carrier-val carriers)])
                    (Extent (interval-around pos)
                            (if (eq? 'f bed)
                                (Interval -inf.0 (car ys))
                                (Interval (car ys) +inf.0))))]
                 [(Knit? op)
                  (let* ([carriers (command-carriers op)]
                         [ys (map Carrier-val carriers)])
                    (Extent (interval-around pos)
                            (if (eq? 'f bed)
                                (Interval -inf.0 (apply max ys))
                                (Interval (apply min ys) +inf.0))))]
                 [(Split? op)
                  (Extent (interval-around pos)
                          (Interval -inf.0 +inf.0))]
                 [(Miss? op)
                  (let* ([carriers (command-carriers op)]
                         [ys (map Carrier-val carriers)])
                    (Extent (interval-around pos)
                            (Interval (car ys) (car ys))))]
                 [(or (In? op)
                      (Out? op))
                  (let* ([dir (op-dir op)]
                         [carriers (command-carriers op)]
                         [ys (map Carrier-val carriers)])
                    (Extent (if (eq? '+ dir)
                                (Interval (+ pos 0.5) (+ pos 0.5))
                                (Interval (- pos 0.5) (- pos 0.5)))
                            (Interval (car ys) (car ys))))]
                 [(Drop? op)
                  (Extent (Interval pos pos)
                          (if (eq? 'f bed)
                              (Interval -inf.0 -inf.0)
                              (Interval +inf.0 +inf.0)))]
                 [(Xfer? op)
                  (if (zero? (get-loops machine needle))
                      #f
                      (Extent (Interval pos pos)
                              (Interval -inf.0 +inf.0)))]
                 [(MOVE? op)
                  (let* ([x (Needle-index needle)]
                         [j (MOVE-j op)]
                         [x-r (- x racking)]
                         [loops (MachineState-loops machine)])                       
                    (if (eq? 'b bed)
                        (let ([y-min (if (zero? (vector-ref (hash-ref loops 'f) ;; assuming typo in paper
                                                            (+ x racking)))
                                         +inf.0
                                         -inf.0)])
                          (Extent (if (positive? j)
                                      (Interval x-r (+ x-r j))
                                      (Interval (+ x-r j) x-r))
                                  (Interval y-min +inf.0)))
                        (let ([y-max (if (zero? (vector-ref (hash-ref loops 'b)
                                                            x-r))
                                         (let* ([carriers (MachineState-carrier-positions machine)]
                                                [cs (sort (hash-keys carriers) >)])
                                           (or (for/or ([c (in-list cs)]) : (Option Positive-Integer)
                                                 (let ([pos (hash-ref carriers c)])
                                                   (if (and (>  pos     x)
                                                            (>= (+ x j) pos))
                                                       c
                                                       #f)))
                                               -inf.0))
                                         +inf.0)])
                          (Extent (if (positive? j)
                                      (Interval x (+ x j))
                                      (Interval (+ x j) x))
                                  (Interval -inf.0 y-max)))))]
                 [else (error 'extent "unknown command")]))]))

(: interval-around : Integer -> Interval)
(define (interval-around pos)
  (Interval (- pos 0.5) (+ pos 0.5)))

(: extents-disjoint? : (Option Extent) (Option Extent) -> Boolean)
(define (extents-disjoint? e1 e2)
  (if (or (false? e1)
          (false? e2))
      #t
      (or (extents-horizontally-disjoint? e1 e2)
          (extents-vertically-disjoint?   e1 e2))))

(: extents-horizontally-disjoint? : Extent Extent -> Boolean)
(define (extents-horizontally-disjoint? e1 e2)
  (intervals-disjoint?
   (Extent-x e1)
   (Extent-x e2)))

(: extents-vertically-disjoint? : Extent Extent -> Boolean)
(define (extents-vertically-disjoint? e1 e2)
  (intervals-disjoint?
   (Extent-y e1)
   (Extent-y e2)))

;; NB intervals are defined to include the boundary,
;; so strict inequality is required
(: intervals-disjoint? : Interval Interval -> Boolean)
(define (intervals-disjoint? i1 i2)
  (or ((Interval-max i1) . < . (Interval-min i2))   ;; not <=
      ((Interval-max i2) . < . (Interval-min i1)))) ;; not <=

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; opposite, consecutive Rack commands cancel
(: merge-rack : (Listof Operation) Natural -> (Listof Operation))
(define (merge-rack ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'merge-rack "merge not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Rack? op1)
                 (Rack? op2)
                 (zero? (+ (Rack-racking op1)
                           (Rack-racking op2)))) ;; equal and opposite
      (error 'merge-rack "merge not possible at position ~a" pos))
    ;; eliminate both
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; when Miss at needle N and carrier C in one direction
;; is followed by Miss at needle N and carrier C in opposite direction,
;; both are eliminated
(: merge-miss : (Listof Operation) Natural -> (Listof Operation))
(define (merge-miss ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'merge-miss "merge not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Miss? op1)
                 (Miss? op2)
                 (equal? (Miss-needle op1)
                         (Miss-needle op2))           ;; same needle
                 (equal? (Miss-carrier op1)
                         (Miss-carrier op2))          ;; same carrier
                 (not (equal? (Miss-direction op1)
                              (Miss-direction op2)))) ;; different directions
      (error 'merge-miss "merge not possible at position ~a" pos))
    ;; eliminate both
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 3
;; eliminates the first of consecutive, opposite Xfers
(: squish : (Listof Operation) Natural -> (Listof Operation))
(define (squish ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'squish "squish not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Xfer? op1)
                 (Xfer? op2)
                 (equal? (Xfer-needle op1)
                         (Xfer-target op2))  ;; same needle
                 (equal? (Xfer-target op1)
                         (Xfer-needle op2))) ;; same needle
      (error 'squish "squish not possible at position ~a" pos))
    ;; eliminate first Xfer
    (append
     (take ops pos)
     (drop ops (+ 1 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 4
;; changes the needle location where Tuck is performed
(: slide : (Listof Operation) Natural -> (Listof Operation))
(define (slide ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'slide "slide not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Tuck? op1)
                 (Xfer? op2) ;; Tuck then Xfer
                 (equal? (Tuck-needle op1)
                         (Xfer-needle op2))) ;; same needle
      (error 'slide "slide not possible at position ~a" pos))
    ;; change location of Tuck
    (append
     (take ops pos)
     (list
      (struct-copy Tuck op1
                   [needle (Xfer-target op2)]))
     (drop ops (+ 1 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 5
;; changes the needle location where Knit or Tuck is performed
;; Table 2
(: conjugate : MachineState (Listof Operation) Natural Dir -> (Listof Operation))
(define (conjugate machine ops pos dir)
  (when (> (add1 pos) (length ops))
    (error 'conjugate "eliminate not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]         
         [ops0 (update! machine0 ops pos)]
         [op (car ops0)]) ;; (list-ref ops pos)
    (unless (or (Knit? op)
                (Tuck? op))
      (error 'conjugate "only Knit and Tuck operations can be conjugated"))
    (let* ([r (MachineState-racking machine0)]
           [d (op-dir op)]
           [n (op-needle op)]
           [cs (command-carriers op)]
           [b (Needle-bed n)]
           [x (Needle-index n)]
           [x-1 (- x 1)]
           [x+1 (+ x 1)]
           [r-1 (- r 1)]
           [r+1 (+ r 1)]
           [n~ (Needle b (if (eq? '- dir) x-1 x+1))]
           [op~ (if (Knit? op)
                    (struct-copy Knit op [needle n~])
                    (struct-copy Tuck op [needle n~]))]
           [conjugated    
            (cond [(and (eq? 'f b)
                        (eq? '+ d)               
                        (eq? '- dir)) ;; Left
                   `(,@(MISS '- (Needle 'f x-1) cs)
                     ;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,op~
                     ,@(MISS '+ (Needle 'f x) cs)
                     ;,(SHIFT n~ r-1 1)
                     ,(MOVE n~ r 1))]

                  [(and (eq? 'f b)
                        (eq? '+ d)
                        (eq? '+ dir)) ;; Right
                   `(;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,@(MISS '+ (Needle 'f x) cs)
                     ,op~
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1)
                     ,@(MISS '- (Needle 'f x+1) cs))]

                  [(and (eq? 'f b)
                        (eq? '- d)
                        (eq? '- dir)) ;; Left
                   `(;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,@(MISS '- (Needle 'f x) cs)
                     ,op~
                     ;,(SHIFT n~ r-1 +1)
                     ,(MOVE n~ r +1)
                     ,@(MISS '+ (Needle 'f x-1) cs))]

                  [(and (eq? 'f b)
                        (eq? '- d)
                        (eq? '+ dir)) ;; Right
                   `(,@(MISS '+ (Needle 'f x+1) cs)
                     ;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,op~
                     ,@(MISS '- (Needle 'f x) cs)
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1))]

                  [(and (eq? 'b b)
                        (eq? '- dir)) ;; Left
                   `(;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,op~
                     ;,(SHIFT n~ r-1 +1)
                     ,(MOVE n~ r +1))]

                  [(and (eq? 'b b)
                        (eq? '+ dir)) ;; Right
                   `(;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,op~
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1))]

                  [else
                   (error 'conjugate "conjugate not possible at position ~a" pos)])])
      (append
       (take ops pos)
       conjugated
       (drop ops (add1 pos))))))

(: MISS : Dir Needle (Listof Carrier) -> (Listof Command))
(define (MISS d n cs)
  (for/list ([c (in-list cs)])
    (Miss (Direction d) n c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra rule:
;; eliminate operations that do nothing.
;; * Nop
;; * Xfer when there are no loops on the source needle
;; * Rack when the racking is the current value

(: eliminate : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (eliminate machine ops pos)
  (unless (< (add1 pos) (length ops))
    (error 'eliminate "eliminate not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]
         [ops0 (update! machine0 ops pos)]
         [op (car ops0)]) ;; (list-ref ops pos)
    (if (or (Nop? op)
            (and (Rack? op)
                 (= (Rack-racking op)
                    (MachineState-racking machine0)))
            (and (Xfer? op)
                 (zero? (get-loops machine0 (op-needle op))))
            (and (MOVE? op)
                 (zero? (get-loops machine0 (op-needle op)))
                 (zero? (get-loops machine0 (op-target op)))))
        (append
         (take ops pos)
         (drop ops (add1 pos)))
        (error 'eliminate "eliminate not possible at position ~a" pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra rule
;; cancel consecutive, opposite Xfer/MOVE when there are no loops on the target
(: cancel : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (cancel machine ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'cancel "cancel not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]
         [ops0 (update! machine0 ops pos)]
         [op1 (first  ops0)]  ;; (list-ref ops0 pos)
         [op2 (second ops0)]) ;; (list-ref ops0 (add1 pos))
    (unless (and (or (and (Xfer? op1)
                          (Xfer? op2))
                     (and (MOVE? op1)
                          (MOVE? op2)))
                 (equal? (op-needle op1)
                         (op-target op2)) ;; same needle
                 (equal? (op-target op1)
                         (op-needle op2)) ;; same needle
                 (zero? (get-loops machine0 (op-target op1)))) ;; no loops on target
      (error 'cancel "cancel not possible at position ~a" pos))
    ;; cancel both operations
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Chunk the Script into Passes. Assumes script is valid.
(: script-passes : Script -> (Listof Pass))
(define (script-passes self)
  (let* ([ops0 (map (λ ([x : Instruction])
                      (Instruction-op x))
                    self)]
         [dir0  (pass-dir  ops0)]
         [type0 (pass-type ops0)])
    (let loop : (Listof Pass)
      ([ops  : (Listof Operation) ops0]
       [dir  : (Option Dir)       dir0]
       [type : PassType           type0]
       [pass : (Listof Operation) null]
       [a    : (Listof Pass)      null])
      (if (null? ops)
          (reverse
           (cons (Pass (reverse pass)
                       dir
                       type)
                 a))
          (let ([op (first ops)])
            (if (and (op-dir? op)
                     (not (eq? (op-dir op)
                               dir)))
                ;; new Pass
                (loop (cdr ops)
                      (op-dir op)
                      'knit
                      (list op)
                      (cons (Pass (reverse pass)
                                  dir
                                  type)
                            a))
                ;; continue
                (loop (cdr ops)
                      dir
                      type
                      (cons op pass)
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
(: foreach-pass : MachineState (Listof Pass) (-> MachineState Pass (Listof Pass)) -> (Listof Pass))
(define (foreach-pass machine passes split-func)
  (let ([machine0 (machine-copy machine)]) ;; copy initial state of machine
    (apply append
           (for/list ([p : Pass (in-list passes)]) : (Listof (Listof Pass))
             (split-func machine0 p)))))

;; splits Pass into one or more Passes
(: pass-split! : MachineState Pass (-> MachineState Pass Pass) (-> Pass (Listof Pass)) -> (Listof Pass))
(define (pass-split! machine pass sort-func split-func)
  (let loop : (Listof Pass)
    ([p0 : Pass          pass]
     [a  : (Listof Pass) null])
    (let* ([p1  (sort-func machine p0)]
           [ps  (split-func p1)]
           [ps1 (first ps)]
           [a1  (cons ps1 a)])
      (run-pass! machine ps1)
      (if (= 1 (length ps))
          ;; not split
          (reverse a1)
          ;; split
          (loop (second ps)
                a1)))))

;; sorts Operations in Pass and updates machine state
(: pass-sort! : MachineState Pass (-> Operation Boolean) -> Pass)
(define (pass-sort! machine pass predicate)
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
                   (swap-possible? machine op1 op2))
              ;; swap
              (begin
                (operate! machine op2)
                (loop (cons op1 (cddr ops))
                      (cons op2 a)))
              ;; don't swap
              (begin
                (operate! machine op1)
                (loop (cdr ops)
                      (cons op1 a))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting Xfers to the end of each pass
;; then splitting them into a separate transfer pass
(: passes-split-xfer : MachineState (Listof Pass) -> (Listof Pass))
(define (passes-split-xfer machine passes)
  (foreach-pass machine passes pass-split-xfer!))

;; sorts Xfers to the end of the pass
;; then splits them into a separate transfer pass
;; and updates machine state
(: pass-split-xfer! : MachineState Pass -> (Listof Pass))
(define (pass-split-xfer! machine self)
  (pass-split! machine self sort-xfer split-xfer))

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
(: sort-xfer : MachineState Pass -> Pass)
(define (sort-xfer machine pass)
  (let ([machine0 (machine-copy machine)] ;; copy initial state of machine
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
                     (swap-possible? machine0 op1 op2))
                ;; swap
                (begin
                  (operate! machine0 op2)
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
                    (let-values ([(head tail) (sort-xfer-aux machine0 ops)])
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
                      (operate! machine0 op1)
                      (loop (cdr ops)
                            (cons op1 a))))))))))

;; Move forward to the next operation that is not an Xfer/Rack
;; and try to sort it to the front of the entire sequence of Xfers.
;; FIXME This assumes that the operations in the Xfer/Rack
;; sequence do not affect machine state in a way that affects
;; the calculation of extents: that assumption may not hold.
(: sort-xfer-aux : MachineState (Listof Operation) -> (values (Listof Operation) (Listof Operation)))
(define (sort-xfer-aux p-machine p-ops)
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
                        (if (swap-possible? p-machine op1 op2)
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
(: passes-split-drop : MachineState (Listof Pass) -> (Listof Pass))
(define (passes-split-drop machine passes)
  (foreach-pass machine passes pass-split-drop!))

;; sorts Drops to the end of the pass
;; then splits them into a separate knit pass
;; and updates machine state
(: pass-split-drop! : MachineState Pass -> (Listof Pass))
(define (pass-split-drop! machine pass)
  (pass-split! machine pass sort-drop! split-drop))

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

;; sorts Drop operations to the end of the pass and updates machine state
(: sort-drop! : MachineState Pass -> Pass)
(define (sort-drop! machine pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (pass-sort! machine pass Drop?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting Out operations to the end of each pass
(: passes-sort-out : MachineState (Listof Pass) -> (Listof Pass))
(define (passes-sort-out machine passes)
  (let ([machine0 (machine-copy machine)]) ;; copy initial state of machine
    (for/list ([p : Pass (in-list passes)]) : (Listof Pass)
      (sort-out! machine0 p))))

;; sorts Out operations to the end of the pass and updates machine state
(: sort-out! : MachineState Pass -> Pass)
(define (sort-out! machine pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (pass-sort! machine pass Out?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterates over passes, sorting In operations to the beginning of each pass
(: passes-sort-in : MachineState (Listof Pass) -> (Listof Pass))
(define (passes-sort-in machine passes)
  (let ([machine0 (machine-copy machine)]) ;; copy initial state of machine
    (for/list ([p : Pass (in-list passes)]) : (Listof Pass)
      (sort-in machine0 p))))

;; sorts In operations to the beginning of the pass
(: sort-in : MachineState Pass -> Pass)
(define (sort-in machine pass)
  (if (not (eq? 'knit (Pass-type pass)))
      pass
      (let* ([ops0 (Pass-ops pass)]
             [ops1 (sort-in-aux machine ops0 null #f)])
        (struct-copy Pass pass
                     [ops ops1]))))

(: sort-in-aux : MachineState (Listof Operation) (Listof Operation) (Option Natural) -> (Listof Operation))
(define (sort-in-aux machine ops head idx)
  (let ([idx0 (index-where ops In?)])
    (if (or (false? idx0)
            (and (not (false? idx))
                 (not (< idx0 idx))))
        (append head ops)
        (if (zero? idx0)
            (sort-in-aux machine
                         (cdr ops)
                         (append head (list (car ops)))
                         #f)
            (let* ([machine0 (machine-copy machine)] ;; copy initial state of machine
                   [ops0 (update! machine0 ops (sub1 idx0))]
                   [op1 (first  ops0)]
                   [op2 (second ops0)])
              (if (swap-possible? machine op1 op2)
                  ;; swap
                  (sort-in-aux machine
                               (append (take ops (sub1 idx0))
                                       (list op2 op1)
                                       (drop ops (add1 idx0)))
                               head
                               idx0)
                  ;; don't swap
                  (sort-in-aux machine
                               (drop ops (sub1 idx0))
                               (append head
                                       (take ops (sub1 idx0)))
                               idx0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: passes-xfer->move : MachineState (Listof Pass) -> (Listof Pass))
(define (passes-xfer->move machine passes)
  (let ([machine0 (machine-copy machine)]) ;; copy initial machine state
    (for/list ([pass (in-list passes)]) : (Listof Pass)
      (let ([pass~ (xfer->move machine0 pass)])
        (run-pass! machine0 pass~)
        pass~))))

;; turns transfer pass featuring blocks of Xfers separated by RACKs
;; into a collection of MOVE operations
(: xfer->move : MachineState Pass -> Pass)
(define (xfer->move machine pass)
  (if (not (eq? 'xfer (Pass-type pass)))
      pass
      (let* ([machine0 (machine-copy machine)] ;; copy initial machine state
             [racking0 (MachineState-racking machine0)]
             [ops      (Pass-ops pass)]
             ;; record Xfers until machine gets back to initial racking
             [machine1 (machine-copy machine0)])
        (let-values ([(failed? end xfers)
                      (let loop : (values Boolean Natural (Listof (Pairof Xfer Integer)))
                        ([lst ops]
                         [i   : Natural 0]
                         [a   : (Listof (Pairof Xfer Integer)) null])
                        (if (null? lst)
                            (values #t 0 null)
                            (let ([op (car lst)])
                              (if (and (or (Rack? op)
                                           (RACK? op))
                                       (= racking0 (op-racking op)))
                                  (values #f (add1 i) (reverse a))
                                  (begin
                                    (unless (or (Rack? op)
                                                (RACK? op)
                                                (Xfer? op))
                                      (values #t 0 null))
                                    (operate! machine1 (car lst))
                                    (if (Xfer? op)
                                        (loop (cdr lst)
                                              (add1 i)
                                              (cons (cons op (MachineState-racking machine1)) a))                          
                                        (loop (cdr lst)
                                              (add1 i)
                                              a)))))))])
          (if failed?
              pass
              (let ([moves
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
                                 (error 'pass-xfer->move "failed: no match for ~a" xfer1)
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
                                   (loop (append
                                          (take (cdr lst) i)
                                          (drop (cdr lst) (add1 i)))
                                         (cons move a)))))))])
                (Pass
                 (append moves
                         (drop ops end))
                 (Pass-dir pass)
                 'move)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; group of instructions in regular sequence across needle bed
;; FIXME pattern matching needs to be a lot more sophisticated than this

(struct Group
  ([op    : Operation]
   [inc   : Integer]
   [count : Positive-Integer])
  #:prefab)

;; constructor
(: make-group : Operation Integer -> Group)
(define (make-group op inc)
  (unless (op-needle? op)
    (error 'make-group "operation ~a cannot form a group" op))
  (Group op inc 1))

(: group-append : Group -> Group)
(define (group-append self)
  (struct-copy Group self
               [inc (add1 (Group-inc self))]))

(: group-ref : Group Natural -> Operation)
(define (group-ref self index)
  (unless (< index (Group-count self))
    (error 'group-ref "index too large for group\nindex: ~a\ncount: ~a" index count))
  (group-member self (* (Group-inc   self)
                        index)))

(: group-next : Group -> Operation)
(define (group-next self)
  (group-member self (* (Group-inc   self)
                        (Group-count self))))

(: group-member : Group Integer -> Operation)
(define (group-member self inc)
  (let* ([op (Group-op self)]
         [n (op-needle op)]
         [x (Needle-index n)]
         [n~ (struct-copy Needle n
                          [index (+ x inc)])])
    (if (op-target? op)
        (let* ([t (op-target op)]
               [y (Needle-index t)]
               [t~ (struct-copy Needle t
                                [index (+ y inc)])])
          (op-copy op
                   #:needle n~
                   #:target t~))
        (op-copy op
                 #:needle n~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; end

;#|
(define machine (make-MachineState 30))

(require "k2f.rkt")
(define script (k2f "../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.k"))
(~>> script
     script-passes
     (passes-split-xfer machine)
     (passes-split-drop machine)
     passes-assign-dir
     ;; FIXME sort xfer, move, and drop Passes according to Dir
     (passes-xfer->move machine)
     (passes-sort-out machine)
     (passes-sort-in machine))
;|#
