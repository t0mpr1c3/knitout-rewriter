#lang typed/racket

;; https://doi.org/10.1145/3592449

(provide swap
         merge-rack
         merge-miss
         squish
         slide
         conjugate)

(require threading)
(require "fnitout-machine.rkt"
         "fnitout-command.rkt"
         "fnitout-validator.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 1
;; knitout operations with disjoint extents can be reordered
(: swap : MachineState (Listof Operation) Natural -> (Listof Operation))
(define (swap machine ops pos)
  (let ([len (length ops)])
    (when (< len (+ 2 pos))
      (error 'fnitout "swap not possible at position ~a" pos))
    (let* ([op1 (list-ref ops pos)]
           [op2 (list-ref ops (add1 pos))]
           [ext1 (extent machine op1)]
           [ext2 (extent machine op2)])
      (unless (extents-disjoint? ext1 ext2)
        (error 'fnitout "swap not possible at position ~a" pos))
      ;; swap
      (append
       (take ops pos)
       (list op2)
       (list op1)
       (drop ops (+ 2 pos))))))

;; definition D.2
;; definition D.6 (MOVE)
(: extent : MachineState Operation -> (Option Extent))
(define (extent machine op)
  (cond [(Nop? op)
         #f]
        [(or (Rack? op)
             (RACK? op)
             (SHIFT? op))
         (Extent (Interval -inf.0 +inf.0)
                 (Interval -inf.0 +inf.0))]
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
                  (let* ([dir (command-dir op)]
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
                  (Extent (Interval pos pos)
                          (Interval -inf.0 +inf.0))]
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
                          (Extent (Interval x-r (+ x-r j))
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
                          (Extent (Interval x (+ x j))
                                  (Interval -inf.0 y-max)))))]
                 [else (error 'fnitout "unknown command")]))]))

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
  (let ([len (length ops)])
    (when (< len (+ 2 pos))
      (error 'fnitout "merge not possible at position ~a" pos))
    (let ([op1 (list-ref ops pos)]
          [op2 (list-ref ops (add1 pos))])
      (unless (and (Rack? op1)
                   (Rack? op2)
                   (zero? (+ (Rack-racking op1)
                             (Rack-racking op2)))) ;; equal and opposite
        (error 'fnitout "merge not possible at position ~a" pos))
      ;; eliminate both
      (append
       (take ops pos)
       (drop ops (+ 2 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; when Miss at needle N and carrier C in one direction
;; is followed by Miss at needle N and carrier C in opposite direction,
;; both are eliminated
(: merge-miss : (Listof Operation) Natural -> (Listof Operation))
(define (merge-miss ops pos)
  (let ([len (length ops)])
    (when (< len (+ 2 pos))
      (error 'fnitout "merge not possible at position ~a" pos))
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
        (error 'fnitout "merge not possible at position ~a" pos))
      ;; eliminate both
      (append
       (take ops pos)
       (drop ops (+ 2 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 3
;; eliminates the first of consecutive, opposite Xfers
(: squish : (Listof Operation) Natural -> (Listof Operation))
(define (squish ops pos)
  (let ([len (length ops)])
    (when (< len (+ 2 pos))
      (error 'fnitout "squish not possible at position ~a" pos))
    (let ([op1 (list-ref ops pos)]
          [op2 (list-ref ops (add1 pos))])
      (unless (and (Xfer? op1)
                   (Xfer? op2)
                   (equal? (Xfer-needle op1)
                           (Xfer-target op2))  ;; same needle
                   (equal? (Xfer-target op1)
                           (Xfer-needle op2))) ;; same needle
        (error 'fnitout "squish not possible at position ~a" pos))
      ;; eliminate first Xfer
      (append
       (take ops pos)
       (drop ops (+ 1 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 4
;; changes the needle location where Tuck is performed
(: slide : (Listof Operation) Natural -> (Listof Operation))
(define (slide ops pos)
  (let ([len (length ops)])
    (when (< len (+ 2 pos))
      (error 'fnitout "slide not possible at position ~a" pos))
    (let ([op1 (list-ref ops pos)]
          [op2 (list-ref ops (add1 pos))])
      (unless (and (Tuck? op1)
                   (Xfer? op2) ;; Tuck then Xfer
                   (equal? (Tuck-needle op1)
                           (Xfer-needle op2))) ;; same needle
        (error 'fnitout "slide not possible at position ~a" pos))
      ;; change location of Tuck
      (append
       (take ops pos)
       (list
        (struct-copy Tuck op1
                     [needle (Xfer-target op2)]))
       (drop ops (+ 1 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 5
;; changes the needle location where Knit or Tuck is performed
;; Table 2
(: conjugate : MachineState Command Dir -> (Listof Operation))
(define (conjugate machine cmd dir)
  (unless (or (Knit? cmd)
              (Tuck? cmd))
    (error 'fnitout "only Knit and Tuck operations can be conjugated"))
  (let* ([r (MachineState-racking machine)]
         [d (command-dir cmd)]
         [n (op-needle cmd)]
         [cs (command-carriers cmd)]
         [b (Needle-bed n)]
         [x (Needle-index n)]
         [x-1 (- x 1)]
         [x+1 (+ x 1)]
         [r-1 (- r 1)]
         [r+1 (+ r 1)]
         [n~ (Needle b (if (eq? '- dir) x-1 x+1))]
         [cmd~ (if (Knit? cmd)
                   (struct-copy Knit cmd [needle n~])
                   (struct-copy Tuck cmd [needle n~]))]) 
    
    (cond [(and (eq? 'f b)
                (eq? '+ d)               
                (eq? '- dir)) ;; Left
           `(,@(MISS '- (Needle 'f x-1) cs)
             ,(SHIFT n r -1)
             ,cmd~
             ,@(MISS '+ (Needle 'f x) cs)
             ,(SHIFT n~ r-1 1))]

          [(and (eq? 'f b)
                (eq? '+ d)
                (eq? '+ dir)) ;; Right
           `(,(SHIFT n r +1)
             ,@(MISS '+ (Needle 'f x) cs)
             ,cmd~
             ,(SHIFT n~ r+1 -1)
             ,@(MISS '- (Needle 'f x+1) cs))]

          [(and (eq? 'f b)
                (eq? '- d)
                (eq? '- dir)) ;; Left
           `(,(SHIFT n r -1)
             ,@(MISS '- (Needle 'f x) cs)
             ,cmd~
             ,(SHIFT n~ r-1 +1)
             ,@(MISS '+ (Needle 'f x-1) cs))]

          [(and (eq? 'f b)
                (eq? '- d)
                (eq? '+ dir)) ;; Right
           `(,@(MISS '+ (Needle 'f x+1) cs)
             ,(SHIFT n r +1)
             ,cmd~
             ,@(MISS '- (Needle 'f x) cs)
             ,(SHIFT n~ r+1 -1))]

          [(and (eq? 'b b)
                (eq? '- dir)) ;; Left
           `(,(SHIFT n r -1)
             ,cmd~
             ,(SHIFT n~ r-1 +1))]

          [(and (eq? 'b b)
                (eq? '+ dir)) ;; Right
           `(,(SHIFT n r +1)
             ,cmd~
             ,(SHIFT n~ r+1 -1))]

          [else null])))

(: MISS : Dir Needle (Listof Carrier) -> (Listof Command))
(define (MISS d n cs)
  (for/list ([c (in-list cs)])
    (Miss (Direction d) n c)))

;; end