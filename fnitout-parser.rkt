#lang racket

;; https://doi.org/10.1145/3592449

(provide (contract-out
          [fnitout-parse (-> string? (listof (cons/c cmd/c string?)))]))

(require brag/support
         racket/syntax
         syntax/parse)
(require "fnitout-grammar.rkt"
         "fnitout-lexer.rkt"
         "fnitout-parse-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse knitout from string
;; returns list of pairs each with car = command and cdr = comment
;; FIXME parse line number of instruction into comment for debugging
(define (fnitout-parse str)
  (let* ([f-input-port (open-input-string (string-downcase str))]
         [f-token-thunk (tokenize f-input-port)]
         [f-stx (parse f-token-thunk)])
    (parse-fnitout f-stx)))

;; tower of macros to process AST

(define (parse-fnitout f-stx)
  (syntax-parse f-stx
    [pattern-stx
     (parse-pattern (syntax->list #'pattern-stx))]))

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
     (parse-command #'command-stx "")]
    [(_ command-stx comment-stx)
     (parse-command #'command-stx (cadr (syntax->datum #'comment-stx)))]))

;; end