#lang racket

;; https://doi.org/10.1145/3592449

(require threading)
(require "../fnitout-command.rkt"
         "../fnitout-parser.rkt"
         "../fnitout-machine.rkt"
         "../fnitout-optimizer.rkt")

(module+ test
  (require rackunit
           threading)

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
   (let ([machine (make-MachineState 10)])
     (set-loops! machine (Needle 'f 1) 1)
     (swap
      machine
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
      0))
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

  ;; section 6.1
  (check-exn
   exn:fail?
   (λ ()
     (let ([machine (make-MachineState 10)])
       (set-loops! machine (Needle 'f 1) 1)
       (swap
        machine
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
        1))))

  ;; Fig. 16
  (check-equal?
   (let ([machine (make-MachineState 10)]) ;; FIXME set loops and carrier position
     (~> '(#s(Xfer #s(Needle f 3)
                   #s(Needle b 3))
           #s(Knit #s(Direction -)
                   #s(Needle b 3)
                   #s(Length 8.0)
                   (#s(Yarn #s(Carrier 3)
                            #s(Length 1.0))))
           #s(Xfer #s(Needle f 3)
                   #s(Needle b 3))
           #s(Xfer #s(Needle f 2)
                   #s(Needle b 2))
           #s(Knit #s(Direction -)
                   #s(Needle b 2)
                   #s(Length 8.0)
                   (#s(Yarn #s(Carrier 3)
                            #s(Length 1.0))))
           #s(Xfer #s(Needle f 2)
                   #s(Needle b 2)))
         (swap machine _ 2)
         (swap machine _ 3)
         (swap machine _ 4)
         (swap machine _ 1)
         (swap machine _ 0)))
   '(#s(Xfer #s(Needle f 2)
             #s(Needle b 2))
     #s(Xfer #s(Needle f 3)
             #s(Needle b 3))
     #s(Knit #s(Direction -)
             #s(Needle b 3)
             #s(Length 8.0)
             (#s(Yarn #s(Carrier 3)
                      #s(Length 1.0))))
     #s(Knit #s(Direction -)
             #s(Needle b 2)
             #s(Length 8.0)
             (#s(Yarn #s(Carrier 3)
                      #s(Length 1.0))))
     #s(Xfer #s(Needle f 2)
             #s(Needle b 2))
     #s(Xfer #s(Needle f 3)
             #s(Needle b 3))))

  #|
  ;; Fig. 18
  (check-equal?
   (let ([machine (make-MachineState 100)])
     (~>
      '(#s(Knit #s(Direction +)
                #s(Needle f 2)
                #s(Length 8.0)
                (#s(Yarn
                   #s(Carrier 3)
                   #s(Length 2.0))))
        #s(Miss #s(Direction +)
                #s(Needle f 1)
                #s(Carrier 3))
        #s(Knit #s(Direction -)
                #s(Needle f 1)
                #s(Length 8.0)
                (#s(Yarn
                   #s(Carrier 3)
                   #s(Length 2.0))))
        #s(Knit #s(Direction -)
                #s(Needle f 2)
                #s(Length 8.0)
                (#s(Yarn
                   #s(Carrier 3)
                   #s(Length 2.0)))))
      (conjugate machine _ 3 '+)
      (conjugate machine _ 0 '+))
|#

  (check-equal?
   (let ([machine (make-MachineState 5)])
     (let-values ([(a b)
                   (sort-xfers-aux
                    machine
                    (list
                     (Xfer (Needle 'f 1)
                           (Needle 'b 1))
                     (Rack 1)
                     (Xfer (Needle 'b 1)
                           (Needle 'f 2))
                     (Rack 0)
                     (Knit (Direction '+)
                           (Needle 'f 2)
                           (Length 1.0)
                           (list
                            (Yarn
                             (Carrier 1)
                             (Length 1.0))))))])
       a))
   (list
    (Knit (Direction '+)
          (Needle 'f 2)
          (Length 1.0)
          (list
           (Yarn
            (Carrier 1)
            (Length 1.0))))
    (Xfer (Needle 'f 1)
          (Needle 'b 1))
    (Rack 1)
    (Xfer (Needle 'b 1)
          (Needle 'f 2))
    (Rack 0)))
  
  (check-equal?
   (let ([machine (make-MachineState 5)])
     (let-values ([(a b)
                   (sort-xfers-aux
                    machine
                    (list
                     (Xfer (Needle 'f 1)
                           (Needle 'b 1))
                     (Rack 1)
                     (Xfer (Needle 'b 1)
                           (Needle 'f 2))
                     (Rack 0)
                     (Knit (Direction '+)
                           (Needle 'b 2)
                           (Length 1.0)
                           (list
                            (Yarn
                             (Carrier 1)
                             (Length 1.0))))))])
       a))
   (list
    (Xfer (Needle 'f 1)
          (Needle 'b 1))
    (Rack 1)
    (Xfer (Needle 'b 1)
          (Needle 'f 2))
    (Rack 0)
    (Knit (Direction '+)
          (Needle 'b 2)
          (Length 1.0)
          (list
           (Yarn
            (Carrier 1)
            (Length 1.0))))))

  (check-equal?
   (let ([machine (make-MachineState 5)])
     (sort-xfers
      machine
      (list
       (Xfer (Needle 'f 1)
             (Needle 'b 1))
       (Miss (Direction '+)
             (Needle 'f 1)
             (Carrier 1))
       (Rack 1)
       (Xfer (Needle 'b 1)
             (Needle 'f 2))
       (Rack 0)
       (Knit (Direction '+)
             (Needle 'f 2)
             (Length 1.0)
             (list
              (Yarn
               (Carrier 1)
               (Length 1.0)))))))
   (list
    (Miss (Direction '+)
          (Needle 'f 1)
          (Carrier 1))
    (Knit (Direction '+)
          (Needle 'f 2)
          (Length 1.0)
          (list
           (Yarn
            (Carrier 1)
            (Length 1.0))))
    (Xfer (Needle 'f 1)
          (Needle 'b 1))
    (Rack 1)
    (Xfer (Needle 'b 1)
          (Needle 'f 2))
    (Rack 0)))

  (check-equal?
   (let ([machine (make-MachineState 5)])
     (pass-sort-in
      machine
      (Pass
       (list
        (Nop)
        (In (Direction '+)
            (Needle 'f 1)
            (Carrier 1))
        (Nop)
        (In (Direction '+)
            (Needle 'f 2)
            (Carrier 2)))
       '+
       'knit)))
   (Pass
    '(#s(In #s(Direction +) #s(Needle f 1) #s(Carrier 1))
      #s(In #s(Direction +) #s(Needle f 2) #s(Carrier 2))
      #s(Nop)
      #s(Nop))
    '+
    'knit))

  (check-equal?   
   (~>
    '(#f #f + #f #f - #f #f #f)
    (assign-dir-aux #f #t null))
   '(+ - + - + - + #f #f))

  (check-equal?
   (~>
    '(#f #f + #f #f - #f #f #f)
    (assign-dir-aux #f #t null)
    (assign-dir-aux #f #t null))
   '(+ - + - + - + - +))

  ;; end of submodule
  )

;; end