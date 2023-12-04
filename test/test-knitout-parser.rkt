#lang racket

(require "../knitout-parser.rkt"
         "../knitout-optimizer.rkt")

(module+ test
  (require rackunit)

  (check-equal?   
   (syntax->datum
    (knitout-parse
     (string-append
      ";comment1\n"
      "pause ;comment2\n"
      "rack -1\n"
      "in 5\n"
      "out 3 2 1\n"
      "drop f+888\n"
      "xfer b-1 b1\n"
      "tuck + f0 5\n"
      "knit + f0 1 1 1 1 1\n"
      "split - f0 b0 12 11 10\n")))
   '((comment ";comment1")
     (pause (comment ";comment2"))
     (rack (racking -1))
     (in (carriers 5))
     (out (carriers 1 2 3))
     (drop (needle f 888))
     (xfer
      (needle b -1)
      (needle b 1))
     (tuck
      (direction +)
      (needle f 0)
      (carriers 5))
     (knit
      (direction +)
      (needle f 0)
      (carriers 1))
     (split
      (direction -)
      (needle f 0)
      (needle b 0)
      (carriers 10 11 12))))

  (check-equal?   
   (syntax->datum
    (split-carriers
     (knitout-parse 
      (string-append
       "out 3 2 1\n")))) ;; split
   '((out (carriers 1))
     (out (carriers 2))
     (out (carriers 3))))

  (check-equal?
   (syntax->datum
    (merge-rack
     (knitout-parse 
      (string-append
       "rack 0\n" ;; merge
       "rack 1\n"))))
   '((rack (racking 1))))

  (check-equal?
   (syntax->datum
    (merge-miss
     (knitout-parse 
      (string-append
       "miss - b1 2\n" ;; merge
       "miss + b1 2\n" ;; merge
       ))))
   null)

  (check-equal?
   (syntax->datum
    (squish
     (knitout-parse 
      (string-append
       "rack 1\n"
       "xfer b0 f1\n" ;; squish
       "xfer f1 b0\n"))))
   '((rack (racking 1))
     (xfer (needle f 1)
           (needle b 0))))

  (check-equal?
   (syntax->datum
    (slide
     (knitout-parse 
      (string-append
       "rack 1\n"
       "tuck + b0 1\n" ;; slide
       "xfer b0 f1\n"))))
   '((rack (racking 1))
     (tuck (direction +)
           (needle f 1)
           (carriers 1))
     (xfer (needle b 0)
           (needle f 1))))

  ;; end of submodule
  )

;; end