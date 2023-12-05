#lang racket

;; https://doi.org/10.1145/3592449

(provide cmd/c
         (contract-out
          [parse-command (-> syntax? string? cmd/c)]))

(require racket/syntax
         syntax/parse
         threading)
(require "fnitout-contracts.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; contract for command

(define cmd/c
  (or/c op/c
        opn/c
        opnt/c
        opndc/c
        opndl/c
        opndly/c
        rack/c
        tuck/c
        split/c
        void?))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse command

(define (parse-command command-stx comment)
  (syntax-parse command-stx
    [(_ {~literal TUCK} dir-stx needle-stx length-stx yarn-stx)
     (Tuck comment
           (parse-needle #'needle-stx)
           (parse-direction #'dir-stx)
           (parse-length #'length-stx)
           (parse-yarn #'yarn-stx))]
    [(_ {~literal KNIT} dir-stx needle-stx length-stx yarn-stxs ...)
     (Knit comment
           (parse-needle #'needle-stx)
           (parse-direction #'dir-stx)
           (parse-length #'length-stx)
           (parse-yarns #'(yarn-stxs ...)))]
    [(_ {~literal SPLIT} dir-stx src-needle-stx dst-needle-stx length-stx yarn-stxs ...)
     (Split comment
            (parse-needle #'src-needle-stx)
            (parse-direction #'dir-stx)
            (parse-length #'length-stx)
            (parse-yarns #'(yarn-stxs ...))
            (parse-needle #'dst-needle-stx))]
    [(_ {~literal MISS} dir-stx needle-stx carrier-stx)
     (Miss comment
           (parse-needle #'needle-stx)
           (parse-direction #'dir-stx)
           (parse-carrier #'carrier-stx))]
    [(_ {~literal IN} dir-stx needle-stx carrier-stx)
     (In  comment
           (parse-needle #'needle-stx)
           (parse-direction #'dir-stx)
           (parse-carrier #'carrier-stx))]
    [(_ {~literal OUT} dir-stx needle-stx carrier-stx)
     (Out  comment
           (parse-needle #'needle-stx)
           (parse-direction #'dir-stx)
           (parse-carrier #'carrier-stx))]
    [(_ {~literal DROP} needle-stx)
     (Drop comment
           (parse-needle #'needle-stx))]
    [(_ {~literal XFER} needle-stx target-stx)
     (Xfer comment
           (parse-needle #'needle-stx)
           (parse-needle #'target-stx))]
    [(_ {~literal RACK} racking-stx)
     (Rack comment
           (parse-racking #'racking-stx))]
    [(_ {~literal NOP})
     (Nop comment)]
    [({~literal comment} comment-stx)
     (Nop (syntax->datum #'comment-stx))]))

;; sort yarns by carrier
(define (parse-yarns yarns-stx)
  (syntax-parse yarns-stx
    [(yarn-stxs ...)
     (~> #'(yarn-stxs ...)
         syntax->list
         (map parse-yarn _)
         ;(filter-new (compose Carrier-val Yarn-carrier))) ;; keep only first of duplicate carriers
         (assert-new (compose Carrier-val Yarn-carrier)) ;; throw error if duplicate carriers
         (sort < #:key (compose Carrier-val Yarn-carrier)))]))

;; flag duplicates as false in list of Booleans
(define (map-duplicate? xs func)
  (let loop ([tail xs]
             [seen (set)]
             [keep null])
    (if (null? tail)
        (reverse keep)
        (let ([k (func (car tail))])
          (if (set-member? seen k)
              ;; reject
              (loop (cdr tail)
                    set
                    (cons #f keep))
              ;; keep
              (loop (cdr tail)
                    (apply set (cons k (set->list seen)))
                    (cons #t keep)))))))

(define (keep xs ks)
  (filter-not false?
              (for/list ([x (in-list xs)]
                         [k (in-list ks)])
                (if k x #f))))

(define (filter-new xs func)
  (keep xs (map-duplicate? xs func)))

(define (assert-new xs func)
  (unless (for/and ([i (in-list (map-duplicate? xs func))]) i)
    (error 'fnitout "duplicate carrier in yarns"))
  xs)

;; end