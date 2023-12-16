#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide swap-possible?
         swap)

(require threading)
(require "fnitout-state.rkt"
         "fnitout-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 1
;; knitout operations with disjoint extents can be reordered
(: swap : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (swap state ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'swap "swap not possible at position ~a" pos))
  (let* ([machine0 (state-copy state)]         
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

;; end
