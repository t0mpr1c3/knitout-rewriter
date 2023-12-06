#lang racket

(provide (all-defined-out))

(require brag/support
         racket/syntax
         syntax/parse
         threading)
(require "knitout-parser.rkt")

;; converts knitout file to formal knitout AST
;; after https://github.com/textiles-lab/fenced-tangle-supplemental/blob/main/k2f.mjs
;;
;; add length annotations as implied
;; use proper synonyms
;; elaborate implied misses into explicit misses
;; divide misses into single-needle steps
;; divide racking into single-needle steps

;; parse knitout into fnitout
;; returns AST
(define (fnitout str)
  (let* ([k-stx (knitout-parse str)] 
         [k-ast (datum->syntax k-stx)])
    (knitout-check-magic-string k-ast)
    (let ([headers (knitout-headers k-ast)])
      k-ast)
    (knitout-remove-comments k-ast)))

(define (knitout-check-magic-string k-ast)
  (unless
      (if (or (null? k-ast)
              (not (eq? 'comment (caar k-ast))))
          #f
          (let ([m (regexp-match #px"^;!knitout-(\\d+)" (cadar k-ast))])
            (if (false? m)
                #f
                (let ([v (string->number (cadr m))])
                  (when (> v 2)
                    (displayln (format "Warning: Knitout is version ~a, but this code only knows about versions up to 2." v)))
                  #t))))
    (error 'fnitout "invalid Knitout magic number string")))

(define (knitout-headers k-ast)
  (let ([headers (make-hasheq)])
    (let loop ([tail (cdr k-ast)])
      (if (null? tail)
          (void)
          (let ([head (car tail)])
            (if (not (eq? 'comment (first head)))
                (void)
                (let ([m (regexp-match #px"^;;(.*?): (.*)" (second head))])
                  (if (false? m)
                      (void)
                      (let* ([h (second m)]
                             [k (string->symbol h)])
                        (if (eq? 'Carriers k)
                            (begin
                              (hash-set! headers k (string-split (third m)))
                              (loop (cdr tail)))
                            (if (eq? 'Gauge k)
                                (let ([g (string->number (third m))])
                                  (if (or (false? g)
                                          (not (real? g))
                                          (not (positive? g)))
                                      (error 'knitout "gauge header value must be greater than zero")
                                      (begin
                                        (hash-set! headers k g)
                                        (loop (cdr tail)))))
                                (begin
                                  ;; ignore these header fields
                                  (unless (or (eq? 'Machine k)
                                              (eq? 'Width k)
                                              (eq? 'Position k)
                                              (string-prefix? h "Yarn-"))
                                    (displayln (format "Warning: Knitout contains unknown header ~a" h)))
                                  (loop (cdr tail))))))))))))
    (unless (hash-has-key? 'Carriers)
      (error 'knitout "Knitout requires Carriers header"))
    (unless (hash-has-key? 'Gauge)
      (hash-set! headers 'Gauge 15))
    ;; assign carriers to positive integers
    (hash-set! headers 'Carriers (range 1 (add1 (length (hash-ref headers 'Carriers)))))
    headers))

(define (knitout-remove-comments k-ast)
  (let loop ([cmds k-ast]
              [acc null])
    (if (null? cmds)
        (reverse acc)
        (let* ([cmd1 (first cmds)]
               [sym1 (first cmd1)])
        (if (eq? 'comment sym1)
            ;; remove comment lines
            (loop (cdr cmds)
                  acc)
            (loop (cdr cmds)
                  (cons cmd1 acc)))))))
