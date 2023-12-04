#lang racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide (all-defined-out))

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre))
(require "knitout-grammar.rkt")

(define-empty-tokens command-tokens (IN INHOOK RELEASEHOOK OUT OUTHOOK STITCH RACK KNIT TUCK SPLIT DROP AMISS XFER MISS PAUSE))
(define-empty-tokens punct-tokens (COMMA LPAREN RPAREN NEWLINE SPACE))

;; assumes all input has been cast to lower case
(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer
     ;; commands
     ["in"
      (token-IN)]
     ["inhook"
      (token-INHOOK)]
     ["releasehook"
      (token-RELEASEHOOK)]
     ["out"
      (token-OUT)]
     ["outhook"
      (token-OUTHOOK)]
     ["stitch"
      (token-STITCH)]
     ["rack"
      (token-RACK)]
     ["knit"
      (token-KNIT)]
     ["tuck"
      (token-TUCK)]
     ["split"
      (token-SPLIT)]
     ["drop"
      (token-DROP)]
     ["amiss"
      (token-AMISS)]
     ["xfer"
      (token-XFER)]
     ["miss"
      (token-MISS)]
     ["pause"
      (token-PAUSE)]
     ;; parameters
     [(:: (:? (:or "+" "-")) (:+ numeric) "." (:* numeric))
      (token 'FLOAT (string->number lexeme))]
     [(:: (:/ "1" "9") (:* numeric))
      (token 'COUNT (string->number lexeme))]
     [(:: (:? (:or "+" "-")) (:+ numeric))
      (token 'INTEGER (string->number lexeme))]
     [(:or "+" "-")
      (token 'DIR lexeme)]
     [(:or "f" "b")
      (token 'BED lexeme)]
     ;; punctuation
     [#\,
      (token-COMMA)]
     [#\(
      (token-LPAREN)]
     [#\)
      (token-RPAREN)]
     [#\newline
      (token-NEWLINE)]
     [whitespace
      (token-SPACE)]
     ;; comment
     [(from/stop-before #\; #\newline)
      (token 'COMMENT lexeme)]
     ;; eof
     [(eof)
      (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

;; end