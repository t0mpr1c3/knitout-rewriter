#lang racket

;; https://doi.org/10.1145/3592449

(require "../fnitout-contracts.rkt"
         "../fnitout-parser.rkt"
         "../fnitout-machine.rkt"
         "../fnitout-optimizer.rkt")

(module+ test
  (require rackunit)

  (check-equal?
   (merge-rack
    (map car
         (fnitout-parse
          (string-append
           "rack 1\n"
           "rack -1\n")))
    0)
   null)

  (check-equal?
   (merge-rack
    (map car
         (fnitout-parse
          (string-append
           "rack 1\n"
           "rack -1\n")))
    0)
   null)

  (check-equal?
   (merge-miss
    (map car
         (fnitout-parse
          (string-append
           "miss - b.1 2\n"
           "miss + b.1 2\n"
           )))
    0)
   null)

  (check-equal?
   (squish
    (map car
         (fnitout-parse
          (string-append
           "rack 1\n"
           "xfer b.0 f.1\n"
           "xfer f.1 b.0\n")))
    1)
   '(#s(Rack 1)
     #s(Xfer #s(Needle f 1)
             #s(Needle b 0))))

  (check-equal?
   (slide
    (map car
         (fnitout-parse
          (string-append
           "rack 1\n"
           "tuck + b.0 1.0 (5, 1.0)\n"
           "xfer b.0 f.1\n")))
    1)
   '(#s(Rack 1)
     #s(Tuck #s(Direction +)
             #s(Needle f 1)
             #s(Length 1.0)
             #s(Yarn #s(Carrier 5)
                     #s(Length 1.0)))
     #s(Xfer #s(Needle b 0)
             #s(Needle f 1))))

  ;; section 6.1
  (check-equal?
   (swap
    (make-MachineState 100)
    '(#s(Knit #s(Direction -)
              #s(Needle f 2)
              #s(Length 3.0)
              (#s(Yarn #s(Carrier 2)
                       #s(Length 1.0))))
      #s(Xfer #s(Needle f 1)
              #s(Needle b 1))
      #s(Miss #s(Direction -)
              #s(Needle f 1)
              #s(Carrier 2)))
    0)
   '(#s(Xfer #s(Needle f 1)
             #s(Needle b 1))
     #s(Knit #s(Direction -)
             #s(Needle f 2)
             #s(Length 3.0)
             (#s(Yarn #s(Carrier 2)
                      #s(Length 1.0))))      
     #s(Miss #s(Direction -)
             #s(Needle f 1)
             #s(Carrier 2))))

  (check-exn
   exn:fail?
   (λ ()
     (swap
      (make-MachineState 100)
      '(#s(Knit #s(Direction -)
                #s(Needle f 2)
                #s(Length 3.0)
                (#s(Yarn #s(Carrier 2)
                         #s(Length 1.0))))
        #s(Xfer #s(Needle f 1)
                #s(Needle b 1))
        #s(Miss #s(Direction -)
                #s(Needle f 1)
                #s(Carrier 2)))
      1)))

  ;; Fig. 16
  (check-equal?
   (~>
    '(#s(Xfer #s(Needle f 3)
              #s(Needle b 3))
      #s(Knit #s(Direction -)
              #s(Needle b.3)
              #s(Length 8.0)
              (#s(Yarn #s(Carrier 3)
                       #s(Length 1.0))))
      #s(Xfer #s(Needle f 3)
              #s(Needle b 3))
      #s(Xfer #s(Needle f 2)
              #s(Needle b 2))
      #s(Knit #s(Direction -)
              #s(Needle b.2)
              #s(Length 8.0)
              (#s(Yarn #s(Carrier 3)
                       #s(Length 1.0))))
      #s(Xfer #s(Needle f 2)
              #s(Needle b 2))
      (swap (make-MachineState 100)
          0)

  ;; end of submodule
  )

;; end