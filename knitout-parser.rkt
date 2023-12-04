#lang racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide knitout-parse)

(require brag/support
         racket/syntax
         syntax/parse
         syntax/warn
         threading)
(require "knitout-grammar.rkt"
         "knitout-lexer.rkt")

;; parse knitout from string
;; returns AST
(define (knitout-parse str)
  (let* ([k-input-port (open-input-string (string-downcase str))]
         [k-token-thunk (tokenize k-input-port)]
         [k-stx (parse k-token-thunk)])
    (parse-knitout k-stx)))

;; tower of macros to process AST

(define (parse-knitout k-stx)
  (syntax-parse k-stx
    [pattern-stx
     (datum->syntax k-stx
                    (parse-pattern (syntax->list #'pattern-stx)))]))

(define (parse-pattern pattern-stx)
  (syntax-parse pattern-stx
    [(_ line-stxs ...)
     (for/list ([line-stx (in-list (syntax->list #'(line-stxs ...)))]
                #:do [(define res (parse-line line-stx))]
                #:when (not (void? res)))
       res)]))

(define (parse-line line-stx)
  (println line-stx)
  (syntax-parse line-stx
    [(_)
     (void)] ;; empty line
    [(_ command-stx)
     (parse-command #'command-stx)]
    [(_ command-stx comment-stx)
     (append
      (parse-command #'command-stx)
      (list (parse-command #'comment-stx)))]))

(define (parse-command command-stx)
  (syntax-parse command-stx
    [({~literal comment} comment-stx)
     `(comment ,(syntax->datum #'comment-stx))]
    [({~literal command} {~literal IN} carrier-stxs ...)
     `(in ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal INHOOK} carrier-stxs ...)
     `(inhook ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal RELEASEHOOK} carrier-stxs ...)
     `(releasehook ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal OUT} carrier-stxs ...)
     `(out ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal OUTHOOK} carrier-stxs ...)
     `(outhook ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal STITCH} size1-stx size2-stx)
     `(stitch ,(syntax->datum #'size1-stx)
              ,(syntax->datum #'size2-stx))]
    [(_ {~literal RACK} rack-stx)
     `(rack (racking ,(syntax->datum #'rack-stx)))]
    [(_ {~literal KNIT} dir-stx needle-stx carrier-stxs ...)
     `(knit ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal TUCK} dir-stx needle-stx carrier-stxs ...)
     `(tuck ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal SPLIT} dir-stx needle1-stx needle2-stx carrier-stxs ...)
     `(split ,(parse-direction #'dir-stx)
             ,(parse-needle #'needle1-stx)
             ,(parse-needle #'needle2-stx)
             ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal DROP} needle-stx)
     `(drop ,(parse-needle #'needle-stx))]
    [(_ {~literal AMISS} needle-stx)
     `(amiss ,(syntax->datum #'needle-stx))]
    [(_ {~literal XFER} needle1-stx needle2-stx)
     `(xfer ,(parse-needle #'needle1-stx)
            ,(parse-needle #'needle2-stx))]
    [(_ {~literal MISS} dir-stx needle-stx carrier-stxs ...)
     `(miss ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carriers #'(carrier-stxs ...)))]
    [(_ {~literal PAUSE})
     `(pause)]))

(define (parse-direction direction-stx)
  (syntax-parse direction-stx
    [(_ dir-stx)
     `(direction ,(string->symbol (syntax->datum #'dir-stx)))]))

(define (parse-needle needle-stx)
  (syntax-parse needle-stx
    [(_ bed-stx index-stx)
     `(needle ,(string->symbol (syntax->datum #'bed-stx))
              ,(syntax->datum #'index-stx))]))

;; sort carriers and remove duplicates
(define (parse-carriers carrierset-stx)
  (syntax-parse carrierset-stx
    [(carrier-stxs ...)
     `(carriers
       ,@(~> #'(carrier-stxs ...)
             syntax->list
             car
             syntax->datum
             cdr
             list->set
             set->list
             (sort <)))]))

;; from alternative specification in https://doi.org/10.1145/3592449
(define (parse-yarn yarn-stx)
  (syntax-parse yarn-stx
    [(_ carrier-stx size-stx)
     `(yarn ,(syntax->datum #'carrier-stx)
            ,(syntax->datum #'size-stx))]))

;; end