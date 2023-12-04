#lang racket

;; https://doi.org/10.1145/3592449

(provide (contract-out
          [fnitout-parse (-> string? syntax?)]))

(require brag/support
         racket/syntax
         syntax/parse)
(require "fnitout-grammar.rkt"
         "fnitout-lexer.rkt"
         "fnitout-commands.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse knitout from string
;; returns formal knitout AST
(define (fnitout-parse str)
  (let* ([fk-input-port (open-input-string (string-downcase str))]
         [fk-token-thunk (tokenize fk-input-port)]
         [fk-stx (parse fk-token-thunk)])
    (parse-fnitout fk-stx)))

;; tower of macros to process AST

(define (parse-fnitout fk-stx)
  (syntax-parse fk-stx
    [pattern-stx
     (datum->syntax fk-stx
                    (parse-pattern (syntax->list #'pattern-stx)))]))

(define (parse-pattern pattern-stx)
  (syntax-parse pattern-stx
    [(_ statement-stx ...)
     (for/list ([statement-stx (in-list (syntax->list #'(statement-stx ...)))]
                #:do [(define res (parse-statement statement-stx))]
                #:when (not (void? res)))
       res)]))

(define (parse-statement statement-stx)
  (syntax-parse statement-stx
    [(_)
     (void)]
    [(_ command-stx)
     (parse-command #'command-stx)]))

;; end