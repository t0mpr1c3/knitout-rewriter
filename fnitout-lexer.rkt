#lang racket

;; https://doi.org/10.1145/3592449

(provide (all-defined-out))

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre))
(require "fnitout-grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-empty-tokens command-tokens (TUCK KNIT SPLIT MISS IN OUT DROP XFER RACK NOP))
(define-empty-tokens punct-tokens (DOT COMMA SEMICOLON LPAREN RPAREN SPACE))

;; assumes all input has been cast to lower case
(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer
     ;; commands
     ["tuck"
      (token-TUCK)]
     ["knit"
      (token-KNIT)]
     ["split"
      (token-SPLIT)]
     ["miss"
      (token-MISS)]
     ["in"
      (token-IN)]
     ["out"
      (token-OUT)]
     ["drop"
      (token-DROP)]
     ["xfer"
      (token-XFER)]
     ["rack"
      (token-RACK)]
     ["nop"
      (token-NOP)]
     ;; numeric
     [(:: (:? "+") (:+ numeric) "." (:* numeric))
      (token 'POSITIVE-FLOAT (string->number lexeme))]
     ["1"
      (token 'ONE 1)]
     [(:: (:/ "1" "9") (:* numeric))
      (token 'COUNT (string->number lexeme))]
     ["-1"
      (token 'MINUS-ONE -1)]
     [(:: (:? (:or "+" "-")) (:+ numeric))
      (token 'INTEGER (string->number lexeme))]
     ;; parameters
     [(:or "+" "-")
      (token 'DIR lexeme)]
     [(:or "f" "b")
      (token 'BED lexeme)]
     ;; punctuation
     [#\.
      (token-DOT)]
     [#\,
      (token-COMMA)]
     [#\;
      (token-SEMICOLON)]
     [#\(
      (token-LPAREN)]
     [#\)
      (token-RPAREN)]
     [whitespace
      (token-SPACE)]
     ;; eof
     [(eof)
      (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

;; end