#lang racket

(require "../knitout-parser.rkt")

(module+ test
  (require rackunit)

  ;(check-equal?
    (knitout-parse
     (string-append
      ";!knitout-2\n"
      ";;Carriers: a b c\n"
      ";comment1\n"
      ";;comment2\n"
      "pause ;comment3\n"
      "rack -1\n"
      "in 5\n"
      "out 3 2 1\n"
      "drop f+888\n"
      "xfer b-1 b1\n"#|
      "tuck + f0 5\n"
      "knit + f0 1 1 1 1 1\n"
      "split - f0 b0 12 11 10\n"|#))
    #|
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
  |#

  ;; end of submodule
  )

;; end