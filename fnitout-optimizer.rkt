#lang racket

;; https://doi.org/10.1145/3592449

(provide merge-rack
         merge-miss
         squish
         slide)

(require racket/syntax
         syntax/parse
         threading)
(require "fnitout-contracts.rkt"
         "fnitout-parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inputs & outputs knitout AST
(define (knitout-optimize f-cmds)
  (~> f-cmds
      merge-rack
      merge-miss
      squish
      slide
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; opposite, consecutive `rack` commands cancel
(define (merge-rack f-cmds)
  (let loop1 ([cmds f-cmds]
              [acc null])
    (if (null? cmds)
        (reverse acc)
        (let ([cmd1 (car cmds)])
          (if (Rack? cmd1)
              (if (null? (cdr cmds))
                  (loop1 null
                         (cons cmd1 acc))
                  (let ([cmd2 (cadr cmds)])
                    (if (not (equal? cmd1
                                     cmd2))
                        ;; merge
                        (loop1 (cddr cmds)
                               acc)
                        ;; next command
                        (loop1 (cdr cmds)
                               (cons cmd1 acc)))))
              (loop1 (cdr cmds)
                     (cons cmd1 acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; when `miss` at needle N and carrier C in one direction
;; is followed by miss at needle N and carrier C in opposite direction,
;; both are eliminated
(define (merge-miss f-cmds)
  (let loop ([cmds f-cmds]
             [acc null])
    (if (null? cmds)
        (reverse acc)
        (let ([head (first cmds)])
          (if (Miss? head)
              (let ([next (second cmds)])
                (if (Miss? next) ;; both `miss` commands
                    (let ([head-dir (Miss-direction head)]
                          [next-dir (Miss-direction next)]
                          [head-needle (Miss-needle head)]
                          [next-needle (Miss-needle next)]
                          [head-carrier (Miss-carrier head)]
                          [next-carrier (Miss-carrier next)])
                      (if (and (equal? head-needle
                                       next-needle)     ;; same needle
                               (equal? head-carrier
                                       next-carrier)    ;; same carrier
                               (not (equal? head-dir
                                            next-dir))) ;; different directions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 3
;; eliminates the first of consecutive, opposite `xfer` commands
(define (squish f-cmds)
  (let loop ([cmds f-cmds]
             [acc null])
    (if (null? cmds)
        (reverse acc)
        (let ([head (first cmds)])
          (if (Xfer? head)
              (let ([next (second cmds)])
                (if (Xfer? next) ;; both `xfer` commands
                    (let ([head-src-needle (Xfer-needle head)]
                          [next-src-needle (Xfer-needle next)]
                          [head-dst-needle (Xfer-target head)]
                          [next-dst-needle (Xfer-target next)])
                      (if (and (equal? head-src-needle
                                       next-dst-needle)  ;; same needle
                               (equal? head-dst-needle
                                       next-src-needle)) ;; same needle
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 4
;; changes the needle location where `tuck` is performed
(define (slide f-cmds)
  (let loop ([cmds f-cmds]
             [acc null])
    (if (null? cmds)
        (reverse acc)
        (let ([head (first cmds)])
          (if (Tuck? head)
              (let ([next (second cmds)])
                (if (Xfer? next) ;; `tuck` then `xfer`
                    (let ([head-needle     (Tuck-needle head)]
                          [next-src-needle (Xfer-needle next)])
                      (if (equal? head-needle
                                  next-src-needle) ;; same needle
                          ;; change location of `tuck`
                          (loop (cddr cmds)
                                (cons next
                                      (cons (struct-copy Tuck head
                                                         [needle (Xfer-target next)])
                                            acc)))
                          ;; next command
                          (loop (cdr cmds)
                                (cons head acc))))
                    (loop (cdr cmds)
                          (cons head acc))))
              (loop (cdr cmds)
                    (cons head acc)))))))

;; end