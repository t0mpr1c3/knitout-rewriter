#lang racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide (all-defined-out))

(require racket/syntax
         syntax/parse
         threading)
(require "knitout-parser.rkt")

;; inputs & outputs knitout AST
(define (knitout-optimize k-stx)
  (~> k-stx
      split-carriers
      merge-rack
      ;merge-miss
      ))

;; split commands that operate on multiple carriers
;; into multiple commands that each operate on a single carrier
(define (split-carriers k-stx)
  (let loop1 ([cmds (syntax->datum k-stx)]
              [acc null])
    (if (null? cmds)
        (datum->syntax k-stx (reverse acc))
        (let* ([cmd1 (car cmds)]
               [sym1 (car cmd1)])
          (if (or (eq? 'in sym1)
                  (eq? 'inhook sym1)
                  (eq? 'releasehook sym1)
                  (eq? 'out sym1)
                  (eq? 'outhook sym1)
                  (eq? 'knit sym1)
                  (eq? 'tuck sym1)
                  (eq? 'split sym1)
                  (eq? 'miss sym1))
              (let-values ([(head1 tail1) (split-at-right cmd1 1)])
                (let loop2 ([cs (cdar tail1)]
                            [acc~ acc])
                  (if (null? cs)
                      (loop1 (cdr cmds)
                             acc~)
                      (loop2 (cdr cs)
                             (cons (append head1 `((carriers ,(car cs))))
                                   acc~)))))
              (loop1 (cdr cmds)
                     (cons cmd1 acc)))))))

;; rewrite rule 2
;; https://doi.org/10.1145/3592449
;; uses only last of consecutive `rack` commands
(define (merge-rack k-stx)
  (let loop1 ([cmds (syntax->datum k-stx)]
              [acc null])
    (if (null? cmds)
        (datum->syntax k-stx (reverse acc))
        (let ([cmd1 (car cmds)])
          (if (eq? 'rack (car cmd1))
              (let loop2 ([head cmd1]
                          [tail (cdr cmds)])
                (if (null? tail)
                    (loop1 null
                           (cons head acc))
                    (let ([next (car tail)])
                      (if (eq? 'rack (car next))
                          (loop2 next
                                 (cdr tail))
                          (loop1 tail
                                 (cons head acc))))))
              (loop1 (cdr cmds)
                     (cons cmd1 acc)))))))

;; rewrite rule 2
;; https://doi.org/10.1145/3592449
;; when `miss` at needle N and carrier C in one direction
;; is followed by miss at needle N and carrier C in opposite direction,
;; eliminates both
(define (merge-miss k-stx)
  (let loop ([cmds (syntax->datum k-stx)]
             [acc null])
    (if (null? cmds)
        (datum->syntax k-stx (reverse acc))
        (let ([head (first cmds)])
          (if (eq? 'miss (first head))
              (let ([next (second cmds)])
                (if (eq? 'miss (car next)) ;; both `miss` commands
                    (let ([head-dir (second head)]
                          [next-dir (second next)]
                          [head-needle (third head)]
                          [next-needle (third next)]
                          [head-carrier (fourth head)]
                          [next-carrier (fourth next)])
                      (if (and (eq? (second head-needle)
                                    (second next-needle))     ;; same needle bed
                               (= (third head-needle)
                                  (third next-needle))        ;; same needle index
                               (= (second head-carrier)
                                  (second next-carrier))      ;; same carrier (assume only 1, i.e. all commands split)
                               (not (eq? (second head-dir)
                                         (second next-dir)))) ;; different directions
                          ;; eliminate both
                          (loop (cddr cmds)
                                acc)
                          ;; next command
                          (loop (cdr cmds)
                                (cons head acc))))
                    (loop (cdr cmds)
                          (cons head acc))))
              (loop (cdr cmds)
                    (cons head acc)))))))

;; rewrite rule 3
;; https://doi.org/10.1145/3592449
;; eliminates the first of consecutive, opposite `xfer` commands
(define (squish k-stx)
  (let loop ([cmds (syntax->datum k-stx)]
             [acc null])
    (if (null? cmds)
        (datum->syntax k-stx (reverse acc))
        (let ([head (first cmds)])
          (if (eq? 'xfer (first head))
              (let ([next (second cmds)])
                (if (eq? 'xfer (first next)) ;; both `xfer` commands
                    (let ([head-needle1 (second head)]
                          [next-needle1 (second next)]
                          [head-needle2 (third head)]
                          [next-needle2 (third next)])
                      (if (and (eq? (second head-needle1)
                                    (second next-needle2)) ;; same needle bed
                               (eq? (third head-needle1)
                                    (third next-needle2))  ;; same needle index
                               (eq? (second head-needle2)
                                    (second next-needle1)) ;; same needle bed
                               (eq? (third head-needle2)
                                    (third next-needle1))) ;; same needle index
                          ;; eliminate first xfer
                          (loop (cddr cmds)
                                (cons next acc))
                          ;; next command
                          (loop (cdr cmds)
                                (cons head acc))))
                    ;; next command
                    (loop (cdr cmds)
                          (cons head acc))))
              ;; next command
              (loop (cdr cmds)
                    (cons head acc)))))))

;; rewrite rule 4
;; https://doi.org/10.1145/3592449
;; changes the needle location where `tuck` is performed
(define (slide k-stx)
  (let loop ([cmds (syntax->datum k-stx)]
             [acc null])
    (if (null? cmds)
        (datum->syntax k-stx (reverse acc))
        (let ([head (first cmds)])
          (if (eq? 'tuck (first head))
              (let ([next (second cmds)])
                (if (eq? 'xfer (first next)) ;; `tuck` then `xfer`
                    (let ([head-needle  (third head)]
                          [next-needle1 (second next)])
                      (if (and (eq? (second head-needle)
                                    (second next-needle1)) ;; same needle bed
                               (eq? (third head-needle)
                                    (third next-needle1))) ;; same needle index
                          ;; change location of `tuck`
                          (loop (cddr cmds)
                                (cons next
                                      (cons (list-set head 2 (third next))
                                            acc)))
                          ;; next command
                          (loop (cdr cmds)
                                (cons head acc))))
                    (loop (cdr cmds)
                          (cons head acc))))
              (loop (cdr cmds)
                    (cons head acc)))))))
