#lang typed/racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Rule
  (U 'KnitCarriage   ;; - Split operation is not permitted
                     ;; - Knit stitches can be combined with either Tuck or Slip
                     ;;     stitches, but not both in the same pass.
     'SingleBed      ;; - No knitting on back bed
                     ;; - Transfers to/from back bed are only allowed under rule 'LaceCarriage
     'SingleColor    ;; - Only one yarn carrier in use at any time
     'LaceCarriage)) ;; - racking can only take values -1, 0, +1

;; end