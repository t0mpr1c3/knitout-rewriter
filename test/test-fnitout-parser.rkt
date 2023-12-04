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
      "tuck + f.0 1 (1,0.0);")))

  ;; duplicate yarn carrier
  (check-exn
   exn:fail?
   (λ ()
     (fnitout-parse
      "knit + f.0 1 (1,1.0)(1,1.0);")))

  (check-equal?
   (parse-carrier
    #'(carrier (positive-integer 1)))
   #s(Carrier 1))

  (check-equal?
   (syntax->datum
    (fnitout-parse
     (string-append
      "nop;"
      "rack 1;"
      "drop f.+888;"
      "xfer f.0 b.1;"
      "in + b.-1 6;"
      "out + f.1 999;"
      "miss - b.1 2;"
      "tuck + f.0 1 (1,1);"
      "knit + f.0 1 (2,1)(1,2.0);"
      "split - f.0 b.0 1 (2, 2.0) (1, 1.0);")))
   '(#s(Rack 1)
     #s(Drop #s(Needle f 888))
     #s(Xfer #s(Needle f 0)
             #s(Needle b 1))
     #s(In #s(Direction +)
           #s(Needle b -1)
           #s(Carrier 6))
     #s(Out #s(Direction +)
            #s(Needle f 1)
            #s(Carrier 999))
     #s(Miss #s(Direction -)
             #s(Needle b 1)
             #s(Carrier 2))
     #s(Tuck #s(Direction +)
             #s(Needle f 0)
             #s(Length 1)
             #s(Yarn #s(Carrier 1)
                     #s(Length 1)))
     #s(Knit #s(Direction +)
             #s(Needle f 0)
             #s(Length 1)
             (#s(Yarn #s(Carrier 1)
                      #s(Length 2.0))
              #s(Yarn #s(Carrier 2)
                      #s(Length 1))))
     #s(Split #s(Direction -)
              #s(Needle f 0)
              #s(Needle b 0)
              #s(Length 1)
              (#s(Yarn #s(Carrier 1)
                       #s(Length 1.0))
               #s(Yarn #s(Carrier 2)
                       #s(Length 2.0))))))

  (check-equal?
   (syntax->datum
    (merge-rack
     (fnitout-parse
      (string-append
       "rack 1;"
       "rack -1;"))))
   null)

  (check-equal?
   (syntax->datum
    (merge-miss
     (fnitout-parse 
      (string-append
       "miss - b.1 2;"
       "miss + b.1 2;"
       ))))
   null)

  (check-equal?
   (syntax->datum
    (squish
     (fnitout-parse 
      (string-append
       "rack 1;"
       "xfer b.0 f.1;"
       "xfer f.1 b.0;"))))
   '(#s(Rack 1)
     #s(Xfer #s(Needle f 1)
             #s(Needle b 0))))

  (check-equal?
   (syntax->datum
    (slide
     (fnitout-parse 
      (string-append
       "rack 1;"
       "tuck + b.0 1.0 (5, 1.0);"
       "xfer b.0 f.1;"))))
   '(#s(Rack 1)
     #s(Tuck #s(Direction +)
             #s(Needle f 1)
             #s(Length 1.0)
             #s(Yarn #s(Carrier 5)
                     #s(Length 1.0)))
     #s(Xfer #s(Needle b 0)
             #s(Needle f 1))))

  ;; end of submodule
  )

;; end