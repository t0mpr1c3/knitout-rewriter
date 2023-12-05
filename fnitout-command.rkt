#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (all-defined-out)
         ;; re-export typed structs
         (struct-out Direction)
         (struct-out Needle)
         (struct-out Carrier)
         (struct-out Length)
         (struct-out Yarn)
         (struct-out Tuck)
         (struct-out Knit)
         (struct-out Split)
         (struct-out Miss)
         (struct-out In)
         (struct-out Out)
         (struct-out Drop)
         (struct-out Xfer)
         (struct-out Rack)
         (struct-out Nop))

(define-type Dir
  (U '+
     '-))

(define-type Bed
  (U 'f
     'b))

(require/typed "fnitout-contracts.rkt"
               [#:struct Direction ([val : Dir])]
               [opposite-dir       (-> Direction Dir)]
               [#:struct Needle    ([bed : Bed]
                                    [index : Natural])]
               [#:struct Carrier   ([val : Positive-Integer])]
               [#:struct Length    ([val : Positive-Float])]
               [#:struct Yarn      ([carrier : Carrier]
                                    [length : Length])]
               [#:struct Tuck      ([direction : Direction]
                                    [needle : Needle]
                                    [length : Length]
                                    [yarn : Yarn]
                                    [comment : String])]
               [#:struct Knit      ([direction : Direction]
                                    [needle : Needle]
                                    [length : Length]
                                    [yarns : (Listof Yarn)]
                                    [comment : String])]
               [#:struct Split     ([direction : Direction]
                                    [needle : Needle]
                                    [target : Needle]
                                    [length : (Option Length)]
                                    [yarns : (Listof Yarn)]
                                    [comment : String])]
               [#:struct Miss      ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier]
                                    [comment : String])]
               [#:struct In        ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier]
                                    [comment : String])]
               [#:struct Out       ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier]
                                    [comment : String])]
               [#:struct Drop      ([needle : Needle]
                                    [comment : String])]
               [#:struct Xfer      ([needle : Needle]
                                    [target : Needle]
                                    [comment : String])]
               [#:struct Rack      ([racking : Natural]
                                    [comment : String])]
               [#:struct Nop       ([comment : String])])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic command

(define-type Command
  (U Tuck
     Knit
     Split
     Miss
     In
     Out
     Drop
     Xfer
     Rack
     Nop))

;; generic command accessors
;; FIXME better without all the conditional statements

(: command-dir : Command -> Dir)
(define (command-dir cmd)
  (unless (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd)
              (Miss? cmd)
              (In? cmd)
              (Out? cmd))
    (error 'fnitout "instruction does not specify a direction"))
  (cond [(Tuck? cmd)  (Direction-val (Tuck-direction cmd))]
        [(Knit? cmd)  (Direction-val (Knit-direction cmd))]
        [(Split? cmd) (Direction-val (Split-direction cmd))]
        [(Miss? cmd)  (Direction-val (Miss-direction cmd))]
        [(In? cmd)    (Direction-val (In-direction cmd))]
        [(Out? cmd)   (Direction-val (Out-direction cmd))]))

(: command-opposite-dir : Command -> Dir)
(define (command-opposite-dir cmd)
  (unless (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd)
              (Miss? cmd)
              (In? cmd)
              (Out? cmd))
    (error 'fnitout "instruction does not specify a direction"))
  (cond [(Tuck? cmd)  (opposite-dir  (Tuck-direction cmd))]
        [(Knit? cmd)  (opposite-dir  (Knit-direction cmd))]
        [(Split? cmd) (opposite-dir  (Split-direction cmd))]
        [(Miss? cmd)  (opposite-dir  (Miss-direction cmd))]
        [(In? cmd)    (opposite-dir  (In-direction cmd))]
        [(Out? cmd)   (Direction-val (Out-direction cmd))]))

(: command-needle : Command -> Needle)
(define (command-needle cmd)
  (unless (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd)
              (Miss? cmd)
              (In? cmd)
              (Out? cmd)
              (Drop? cmd)
              (Xfer? cmd))
    (error 'fnitout "instruction does not specify a needle"))
  (cond [(Tuck? cmd)  (Tuck-needle cmd)]
        [(Knit? cmd)  (Knit-needle cmd)]
        [(Split? cmd) (Split-needle cmd)]
        [(Miss? cmd)  (Miss-needle cmd)]
        [(In? cmd)    (In-needle cmd)]
        [(Out? cmd)   (Out-needle cmd)]
        [(Drop? cmd)  (Drop-needle cmd)]
        [(Xfer? cmd)  (Xfer-needle cmd)]))

(: command-target : Command -> Needle)
(define (command-target cmd)
  (unless (or (Split? cmd)
              (Xfer? cmd))
    (error 'fnitout "instruction does not specify a target needle"))
  (cond [(Split? cmd) (Split-target cmd)]
        [(Xfer? cmd)  (Xfer-target cmd)]))

(: command-length : Command -> (Option Length))
(define (command-length cmd)
  (unless (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd))
    (error 'fnitout "instruction does not specify a loop length"))
  (cond [(Tuck? cmd)  (Tuck-length cmd)]
        [(Knit? cmd)  (Knit-length cmd)]
        [(Split? cmd) (Split-length cmd)]))

(: command-carriers : Command -> (Listof Carrier))
(define (command-carriers cmd)
  (unless (or (Tuck? cmd)
              (Knit? cmd)
              (Split? cmd)
              (Miss? cmd)
              (In? cmd)
              (Out? cmd))
    (error 'fnitout "instruction does not specify any carriers"))
  (cond [(Tuck? cmd)  (list (Yarn-carrier (Tuck-yarn cmd)))]
        [(Knit? cmd)  (map Yarn-carrier (Knit-yarns cmd))]
        [(Split? cmd) (map Yarn-carrier (Split-yarns cmd))]
        [(Miss? cmd)  (list (Miss-carrier cmd))]
        [(In? cmd)    (list (In-carrier cmd))]
        [(Out? cmd)   (list (Out-carrier cmd))]))

;; end