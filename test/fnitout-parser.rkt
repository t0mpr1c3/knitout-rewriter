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
     "nop\n"
     "nop;a\n"
     ";b\n"
     "rack 1   ;abc \n"
     "drop f.+888\n"
     "xfer f.0 b.1\n"
     "in + b.-1 6\n"
     "out + f.1 999\n"
     "miss - b.1 2\n"
     "tuck + f.0 1 (1,1)\n"
     "knit + f.0 1 (2,1)(1,2.0)\n"
     "split - f.0 b.0 1 (2, 2.0) (1, 1.0)\n"))
   '(#s((Nop Op 1)
        "")
     #s((Nop Op 1)
        ";a")
     #s((Nop Op 1)
        ";b")
     #s((Rack Op 1)
        ";abc "
        1)
     #s((Drop OpN 1 Op 1)
        ""
        #s(Needle f 888))
     #s((Xfer OpNT 1 OpN 1 Op 1)
        ""
        #s(Needle f 0)
        #s(Needle b 1))
     #s((In OpNDC 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle b -1)
        #s(Direction +)
        #s(Carrier 6))
     #s((Out OpNDC 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle f 1)
        #s(Direction +)
        #s(Carrier 999))
     #s((Miss OpNDC 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle b 1)
        #s(Direction -)
        #s(Carrier 2))
     #s((Tuck OpNDL 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle f 0)
        #s(Direction +)
        #s(Length 1)
        #s(Yarn #s(Carrier 1)
                #s(Length 1)))
     #s((Knit OpNDLY 1 OpNDL 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle f 0)
        #s(Direction +)
        #s(Length 1)
        (#s(Yarn #s(Carrier 1)
                 #s(Length 2.0))
         #s(Yarn #s(Carrier 2)
                 #s(Length 1))))
     #s((Split OpNDLY 1 OpNDL 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle f 0)
        #s(Direction -)
        #s(Length 1)
        (#s(Yarn #s(Carrier 1)
                 #s(Length 1.0))
         #s(Yarn #s(Carrier 2)
                 #s(Length 2.0)))
        #s(Needle b 0))))

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
   '(#s((Rack Op 1)
        ""
        1)
     #s((Xfer OpNT 1 OpN 1 Op 1)
        ""
        #s(Needle f 1)
        #s(Needle b 0))))

  (check-equal?
   (slide
    (fnitout-parse 
     (string-append
      "rack 1\n"
      "tuck + b.0 1.0 (5, 1.0)\n"
      "xfer b.0 f.1\n")))
   '(#s((Rack Op 1)
        ""
        1)
     #s((Tuck OpNDL 1 OpND 1 OpN 1 Op 1)
        ""
        #s(Needle f 1)
        #s(Direction +)
        #s(Length 1.0)
        #s(Yarn #s(Carrier 5)
                #s(Length 1.0)))
     #s((Xfer OpNT 1 OpN 1 Op 1)
        ""
        #s(Needle b 0)
        #s(Needle f 1))))

  ;; end of submodule
  )

;; end