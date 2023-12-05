#lang racket

;; https://doi.org/10.1145/3592449

(require "../fnitout-contracts.rkt"
         "../fnitout-parser.rkt"
         "../fnitout-optimizer.rkt")

(module+ test
  (require rackunit)

  (check-equal?
   (parse-racking
    #'(racking -1))
   -1)
  
  (check-equal?
   (parse-needle
    #'(needle "f" (integer 0)))
   (Needle 'f 0))
  
  (check-equal?
   (parse-length
    #'(length (positive-float 1.0)))
   #s(Length 1.0))

  ;; length must be positive
  (check-exn
   exn:fail?
   (λ ()
     (parse-length
      #'(length (positive-float 0.0)))))

  ;; length must be positive
  (check-exn
   exn:fail?
   (λ ()
     (fnitout-parse
      "tuck + f.0 1 (1,0.0)\n")))

  ;; duplicate yarn carrier
  (check-exn
   exn:fail?
   (λ ()
     (fnitout-parse
      "knit + f.0 1 (1,1.0)(1,1.0)\n")))

  (check-equal?
   (parse-carrier
    #'(carrier (positive-integer 1)))
   #s(Carrier 1))

  (check-equal?
   (fnitout-parse
    (string-append
     ";a\n"
     "nop\n"
     "nop ;b\n"
     "rack 1\n"
     "drop f.+888\n"
     "xfer f.0 b.1\n"
     "in + b.-1 6\n"
     "out + f.1 999\n"
     "miss - b.1 2\n"
     "tuck + f.0 1 (1,1)\n"
     "knit + f.0 1 (2,1)(1,2.0)\n"
     "split - f.0 b.0 1 (2, 2.0) (1, 1.0)\n"))
   '(#s(Nop
        ";a")
     #s(Nop
        "")
     #s(Nop
        ";b")
     #s(Rack 1
             "")
     #s(Drop #s(Needle f 888)
             "")
     #s(Xfer #s(Needle f 0)
             #s(Needle b 1)
             "")
     #s(In #s(Direction +)
           #s(Needle b -1)
           #s(Carrier 6)
           "")
     #s(Out #s(Direction +)
            #s(Needle f 1)
            #s(Carrier 999)
            "")
     #s(Miss #s(Direction -)
             #s(Needle b 1)
             #s(Carrier 2)
             "")
     #s(Tuck #s(Direction +)
             #s(Needle f 0)
             #s(Length 1)
             #s(Yarn #s(Carrier 1)
                     #s(Length 1))
             "")
     #s(Knit #s(Direction +)
             #s(Needle f 0)
             #s(Length 1)
             (#s(Yarn #s(Carrier 1)
                      #s(Length 2.0))
              #s(Yarn #s(Carrier 2)
                      #s(Length 1)))
             "")
     #s(Split #s(Direction -)
              #s(Needle f 0)
              #s(Needle b 0)
              #s(Length 1)
              (#s(Yarn #s(Carrier 1)
                       #s(Length 1.0))
               #s(Yarn #s(Carrier 2)
                       #s(Length 2.0)))
              "")))

  (check-equal?
   (merge-rack
    (fnitout-parse
     (string-append
      "rack 1\n"
      "rack -1\n")))
   null)

  (check-equal?
   (merge-miss
    (fnitout-parse
     (string-append
      "miss - b.1 2\n"
      "miss + b.1 2\n"
      )))
   null)

  (check-equal?
   (squish
    (fnitout-parse
     (string-append
      "rack 1\n"
      "xfer b.0 f.1\n"
      "xfer f.1 b.0\n")))
   '(#s(Rack 1
             "")
     #s(Xfer #s(Needle f 1)
             #s(Needle b 0)
             "")))

  (check-equal?
   (slide
    (fnitout-parse
     (string-append
      "rack 1\n"
      "tuck + b.0 1.0 (5, 1.0)\n"
      "xfer b.0 f.1\n")))
   '(#s(Rack 1
             "")
     #s(Tuck #s(Direction +)
             #s(Needle f 1)
             #s(Length 1.0)
             #s(Yarn #s(Carrier 5)
                     #s(Length 1.0))
             "")
     #s(Xfer #s(Needle b 0)
             #s(Needle f 1)
             "")))

  ;; end of submodule
  )

;; end