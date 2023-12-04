#lang typed/racket

;; https://doi.org/10.1145/3592449

(provide fnitout-parse)

(require brag/support
         racket/syntax
         syntax/parse
         syntax/warn
         threading)
(require "fnitout-grammar.rkt"
         "fnitout-lexer.rkt")

;; parse knitout from string
;; returns formal AST
(: fnitout-parse : String -> Syntax)
(define (fnitout-parse str)
  (let* ([fk-input-port (open-input-string (string-downcase str))]
         [fk-token-thunk (tokenize fk-input-port)]
         [fk-stx (parse fk-token-thunk)])
    (parse-fnitout fk-stx)))

;; tower of macros to process AST

(: parse-fnitout : Syntax -> Syntax)
(define (parse-fnitout fk-stx)
  (syntax-parse fk-stx
    [pattern-stx
     (datum->syntax fk-stx
                    (parse-pattern (syntax->list #'pattern-stx)))]))

(: parse-pattern : Syntax -> Syntax)
(define (parse-pattern pattern-stx)
  (syntax-parse pattern-stx
    [(_ statement-stx ...)
     (for/list ([statement-stx (in-list (syntax->list #'(statement-stx ...)))]
                #:do [(define res (parse-statement statement-stx))]
                #:when (not (void? res)))
       res)]))

(: parse-statement : Syntax -> Syntax)
(define (parse-statement statement-stx)
  (syntax-parse statement-stx
    [(_)
     (void)]
    [(_ command-stx)
     (parse-command #'command-stx)]))

(: parse-command : Syntax -> Syntax)
(define (parse-command command-stx)
  (syntax-parse command-stx
    [(_ {~literal TUCK} dir-stx needle-stx length-stx yarn-stx)
     `(tuck ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-length #'length-stx)
            ,(parse-yarn #'yarn-stx))]
    [(_ {~literal KNIT} dir-stx needle-stx length-stx yarn-stxs ...)
     `(knit ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-length #'length-stx)
            ,(parse-yarns #'(yarn-stxs ...)))]
    [(_ {~literal SPLIT} dir-stx needle1-stx needle2-stx length-stx yarn-stxs ...)
     `(split ,(parse-direction #'dir-stx)
             ,(parse-needle #'needle1-stx)
             ,(parse-needle #'needle2-stx)
            ,(parse-length #'length-stx)
            ,(parse-yarns #'(yarn-stxs ...)))]
    [(_ {~literal MISS} dir-stx needle-stx carrier-stx)
     `(miss ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carrier #'carrier-stx))]
    [(_ {~literal IN} dir-stx needle-stx carrier-stx)
     `(in ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carrier #'carrier-stx))]
    [(_ {~literal OUT} dir-stx needle-stx carrier-stx)
     `(out ,(parse-direction #'dir-stx)
            ,(parse-needle #'needle-stx)
            ,(parse-carrier #'carrier-stx))]
    [(_ {~literal DROP} needle-stx)
     `(drop ,(parse-needle #'needle-stx))]
    [(_ {~literal XFER} needle1-stx needle2-stx)
     `(xfer ,(parse-needle #'needle1-stx)
            ,(parse-needle #'needle2-stx))]
    [(_ {~literal RACK} rack-stx)
     `(rack ,(parse-racking #'rack-stx))]
    [(_ {~literal NOP})
     (void)]))

(: parse-racking : Syntax -> Syntax)
(define (parse-racking rack-stx)
  (syntax-parse rack-stx
    [(_ r-stx)
     `(racking ,(syntax->datum #'r-stx))]))

(: parse-direction : Syntax -> Syntax)
(define (parse-direction direction-stx)
  (syntax-parse direction-stx
    [(_ dir-stx)
     `(direction ,(string->symbol (syntax->datum #'dir-stx)))]))

(: parse-needle : Syntax -> Syntax)
(define (parse-needle needle-stx)
  (syntax-parse needle-stx
    [(_ bed-stx index-stx)
     `(needle ,(string->symbol (syntax->datum #'bed-stx))
              ,(cadr (syntax->datum #'index-stx)))]))

(: parse-carrier : Syntax -> Syntax)
(define (parse-carrier carrier-stx)
  (syntax-parse carrier-stx
    [(_ carrier-stx)
     `(carrier ,(cadr (syntax->datum #'carrier-stx)))]))

(: parse-length : Syntax -> Syntax)
(define (parse-length length-stx)
  (syntax-parse length-stx
    [(_ size-stx)
     `(length ,(cadr (syntax->datum #'size-stx)))]))

(: parse-yarn : Syntax -> Syntax)
(define (parse-yarn yarn-stx)
  (syntax-parse yarn-stx
    [(_ carrier-stx length-stx)
     `(yarn ,(parse-carrier #'carrier-stx)
            ,(parse-length #'length-stx))]))

;; sort yarns by carrier
(: parse-yarns : Syntax -> Syntax)
(define (parse-yarns yarns-stx)
  (syntax-parse yarns-stx
    [(yarn-stxs ...)
     `(yarns
       ,@(~> #'(yarn-stxs ...)
             syntax->list
             (map parse-yarn _)
             ;(filter-new cadadr) ;; keep only first of duplicate carriers
             (assert-new cadadr) ;; throw error if duplicate carriers
             (sort < #:key cadadr)))]))

;; flag duplicates as false in list of Booleans
(: map-duplicate? : (Listof Natural) (Any -> Natural) -> (Listof Boolean))
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

(: keep : (Listof Natural) (Listof Boolean) -> (Listof Natural))
(define (keep xs ks)
  (filter-not false?
              (for/list ([x (in-list xs)]
                         [k (in-list ks)])
                (if k x #f))))

(: filter-new : (Listof Natural) (Any -> Natural) -> (Listof Natural))
(define (filter-new xs func)
  (keep xs (map-duplicate? xs func)))

(: assert-new : (Listof Natural) -> (Listof Natural))
(define (assert-new xs func)
  (unless (for/and ([i (in-list (map-duplicate? xs func))]) i)
    (error 'fnitout "duplicate carrier in yarns"))
  xs)

;; end