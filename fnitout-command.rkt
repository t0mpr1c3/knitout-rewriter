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
         (struct-out Op)
         (struct-out OpN)
         (struct-out OpNT)
         (struct-out OpND)
         (struct-out OpNDC)
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
               [#:struct Direction      ([val : Dir])]
               [opposite-dir            (-> Direction Dir)]
               [#:struct Needle         ([bed : Bed]
                                         [index : Natural])]
               [#:struct Carrier        ([val : Positive-Integer])]
               [#:struct Length         ([val : Positive-Float])]
               [#:struct Yarn           ([carrier : Carrier]
                                         [length : Length])]
               [#:struct Op             ([comment : String])]
               [#:struct (OpN Op)       ([needle : Needle])]
               [#:struct (OpNT OpN)     ([target : Needle])]
               [#:struct (OpND OpN)     ([direction : Direction])]
               [#:struct (OpNDC OpND)   ([carrier : Carrier])]
               [#:struct (OpNDL OpND)   ([length : Length])]
               [#:struct (OpNDLY OpNDL) ([yarns : (Listof Yarn)])]
               [#:struct (Tuck OpNDL)   ([yarn : Yarn])]
               [#:struct (Knit OpNDLY)  ()]
               [#:struct (Split OpNDLY) ([target : Needle])]
               [#:struct (Miss OpNDC)   ()]
               [#:struct (In OpNDC)     ()]
               [#:struct (Out OpNDC)    ()]
               [#:struct (Drop OpN)     ()]
               [#:struct (Xfer OpNT)    ()]
               [#:struct (Rack Op)      ([racking : Natural])]
               [#:struct (Nop Op)       ()])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic command

(define-type Command
  (U Op
     OpN
     OpNT
     OpNDC
     OpNDLY
     Rack
     Tuck
     Split))

;; generic command accessors

(: command-dir : Command -> Dir)
(define (command-dir cmd)
  (unless (OpND? cmd)
    (error 'fnitout "instruction does not specify a direction"))
  (Direction-val (OpND-direction cmd)))

(: command-opposite-dir : Command -> Dir)
(define (command-opposite-dir cmd)
  (unless (OpND? cmd)
    (error 'fnitout "instruction does not specify a direction"))
  (cond [(Out? cmd) (Direction-val (OpND-direction cmd))]
        [else       (opposite-dir  (OpND-direction cmd))]))

(: command-needle : Command -> Needle)
(define (command-needle cmd)
  (unless (OpN? cmd)
    (error 'fnitout "instruction does not specify a needle"))
  (OpN-needle cmd))

(: command-target : Command -> Needle)
(define (command-target cmd)
  (unless (or (Split? cmd)
              (OpNT? cmd))
    (error 'fnitout "instruction does not specify a target needle"))
  (cond [(Split? cmd) (Split-target cmd)]
        [else         (OpNT-target cmd)]))

(: command-length : Command -> (Option Length))
(define (command-length cmd)
  (unless (OpNDL? cmd)
    (error 'fnitout "instruction does not specify a loop length"))
  (OpNDL-length cmd))

(: command-carriers : Command -> (Listof Carrier))
(define (command-carriers cmd)
  (unless (or (Tuck? cmd)
              (OpNDLY? cmd)
              (OpNDC? cmd))
    (error 'fnitout "instruction does not specify any carriers"))
  (cond [(Tuck? cmd)   (list (Yarn-carrier (Tuck-yarn cmd)))]
        [(OpNDLY? cmd) (map Yarn-carrier (OpNDLY-yarns cmd))]
        [else          (list (OpNDC-carrier cmd))]))

;; end