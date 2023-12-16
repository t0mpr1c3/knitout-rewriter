#lang typed/racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Rule
  (U 'NoSplit        ;; Split operation is not permitted
     'SingleBed      ;; no knitting on back bed, transfers to/from back bed are only allowed under rule 'LaceCarriage
     'SingleColor    ;; Only one yarn carrier in use at any time
     'LaceCarriage)) ;; racking can only take values -1, 0, +1

;; end