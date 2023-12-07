#lang typed/racket

;; https://doi.org/10.1145/3592449

(provide merge-rack
         merge-miss
         squish
         slide)

(require racket/syntax
         syntax/parse
         threading)
(require "fnitout-command.rkt"
         "fnitout-validator.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 1
;; knitout operations with disjoint extents can be reordered
;; TODO: MOVE 

(: extent : Integer Command -> (Option Extent))
(define (extent racking cmd)
  (cond [(Nop? cmd)
         #f]
        [(Rack? cmd)
         (Extent (Interval -inf.0 +inf.0)
                 (Interval -inf.0 +inf.0))]
        [else
         (let* ([dir (command-dir cmd)]
                [needle (command-needle cmd)]
                [bed (Needle-bed needle)]
                [pos (needle-physical-position
                      racking
                      needle)]
                [carriers (command-carriers cmd)]
                [ys (map Carrier-val carriers)])
               (cond [(Tuck? cmd)
                      (Extent (interval-around pos)
                              (if (eq? 'f bed)
                                  (Interval -inf.0 (car ys))
                                  (Interval (car ys) +inf.0)))]
                     [(Knit? cmd)
                      (Extent (interval-around pos)
                              (if (eq? 'f bed)
                                  (Interval -inf.0 (apply max ys))
                                  (Interval (apply min ys) +inf.0)))]
                     [(Split? cmd)
                      (Extent (interval-around pos)
                              (Interval -inf.0 +inf.0))]
                     [(Miss? cmd)
                      (Extent (interval-around pos)
                              (Interval (car ys) (car ys)))]
                     [(or (In? cmd)
                          (Out? cmd))
                      (Extent (if (eq? '+ dir)
                                  (Interval (+ pos 0.5) (+ pos 0.5))
                                  (Interval (- pos 0.5) (- pos 0.5)))
                              (Interval (car ys) (car ys)))]
                     [(Drop? cmd)
                      (Extent (Interval pos pos)
                              (if (eq? 'f bed)
                                  (Interval -inf.0 -inf.0)
                                  (Interval +inf.0 +inf.0)))]
                     [(Xfer? cmd)
                      (Extent (Interval pos pos)
                              (Interval -inf.0 +inf.0))]
                     [else (error 'fnitout "unknown command")]))]))

(: interval-around : Integer -> Interval)
(define (interval-around pos)
  (Interval (- pos 0.5) (+ pos 0.5)))

(: extents-disjoint? : Extent Extent -> Boolean)
(define (extents-disjoint? e1 e2)
  (or (extents-horizontally-disjoint? e1 e2)
      (extents-vertically-disjoint?   e1 e2)))

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
(: merge-rack : (Listof Command) Natural -> (Listof Command))
(define (merge-rack cmds pos)
  (let ([len (length cmds)])
    (when (< len (+ 2 pos))
      (error 'fnitout "merge not possible at position ~a" pos))
    (let ([cmd1 (list-ref cmds pos)]
          [cmd2 (list-ref cmds (add1 pos))])
      (unless (and (Rack? cmd1)
                   (Rack? cmd2)
                   (zero? (+ (Rack-racking cmd1)
                             (Rack-racking cmd2)))) ;; equal and opposite
        (error 'fnitout "merge not possible at position ~a" pos))
      ;; eliminate both
      (append
       (take cmds pos)
       (drop cmds (+ 2 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; when Miss at needle N and carrier C in one direction
;; is followed by Miss at needle N and carrier C in opposite direction,
;; both are eliminated
(: merge-miss : (Listof Command) Natural -> (Listof Command))
(define (merge-miss cmds pos)
  (let ([len (length cmds)])
    (when (< len (+ 2 pos))
      (error 'fnitout "merge not possible at position ~a" pos))
    (let ([cmd1 (list-ref cmds pos)]
          [cmd2 (list-ref cmds (add1 pos))])
      (unless (and (Miss? cmd1)
                   (Miss? cmd2)
                   (equal? (Miss-needle cmd1)
                           (Miss-needle cmd2))           ;; same needle
                   (equal? (Miss-carrier cmd1)
                           (Miss-carrier cmd2))          ;; same carrier
                   (not (equal? (Miss-direction cmd1)
                                (Miss-direction cmd2)))) ;; different directions
      (error 'fnitout "merge not possible at position ~a" pos))
      ;; eliminate both
      (append
       (take cmds pos)
       (drop cmds (+ 2 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 3
;; eliminates the first of consecutive, opposite Xfers
(: squish : (Listof Command) Natural -> (Listof Command))
(define (squish cmds pos)
  (let ([len (length cmds)])
    (when (< len (+ 2 pos))
      (error 'fnitout "squish not possible at position ~a" pos))
    (let ([cmd1 (list-ref cmds pos)]
          [cmd2 (list-ref cmds (add1 pos))])
      (unless (and (Xfer? cmd1)
                   (Xfer? cmd2)
                   (equal? (Xfer-needle cmd1)
                           (Xfer-target cmd2))  ;; same needle
                   (equal? (Xfer-target cmd1)
                           (Xfer-needle cmd2))) ;; same needle
      (error 'fnitout "squish not possible at position ~a" pos))
      ;; eliminate first Xfer
      (append
       (take cmds pos)
       (drop cmds (+ 1 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 4
;; changes the needle location where Tuck is performed
(: slide : (Listof Command) Natural -> (Listof Command))
(define (slide cmds pos)
  (let ([len (length cmds)])
    (when (< len (+ 2 pos))
      (error 'fnitout "slide not possible at position ~a" pos))
    (let ([cmd1 (list-ref cmds pos)]
          [cmd2 (list-ref cmds (add1 pos))])
      (unless (and (Tuck? cmd1)
                   (Xfer? cmd2) ;; Tuck then Xfer
                   (equal? (Tuck-needle cmd1)
                           (Xfer-needle cmd2))) ;; same needle
      (error 'fnitout "slide not possible at position ~a" pos))
      ;; change location of Tuck
      (append
       (take cmds pos)
       (list
        (struct-copy Tuck cmd1
                     [needle (Xfer-target cmd2)]))
       (drop cmds (+ 1 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 5
;; changes the needle location where Knit or Tuck is performed
(: conjugate : (Listof Command) Natural -> (Listof Command))
(define (conjugate cmds pos)
  cmds) ;; placeholder

;; end