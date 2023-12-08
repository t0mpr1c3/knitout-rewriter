#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (all-defined-out)
         ;; re-export typed structs
         (struct-out Direction)
         (struct-out Needle)
         (struct-out Carrier)
         (struct-out Length)
         (struct-out Interval)
         (struct-out Extent)
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
(define Dir? (make-predicate Bed))

(define-type Bed
  (U 'f
     'b))
(define Bed? (make-predicate Bed))

(require/typed "fnitout-contracts.rkt"
               [#:struct Direction ([val : Dir])]
               [opposite-dir       (-> Direction Dir)]
               [#:struct Needle    ([bed : Bed]
                                    [index : Integer])]
               [#:struct Carrier   ([val : Positive-Integer])]
               [#:struct Length    ([val : Positive-Float])]
               [#:struct Yarn      ([carrier : Carrier]
                                    [length : Length])]
               [#:struct Interval  ([min : Real]
                                    [max : Real])]
               [#:struct Extent    ([x : Interval]
                                    [y : Interval])]
               [#:struct Tuck      ([direction : Direction]
                                    [needle : Needle]
                                    [length : Length]
                                    [yarn : Yarn])]
               [#:struct Knit      ([direction : Direction]
                                    [needle : Needle]
                                    [length : Length]
                                    [yarns : (Listof Yarn)])]
               [#:struct Split     ([direction : Direction]
                                    [needle : Needle]
                                    [target : Needle]
                                    [length : Length]
                                    [yarns : (Listof Yarn)])]
               [#:struct Miss      ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier])]
               [#:struct In        ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier])]
               [#:struct Out       ([direction : Direction]
                                    [needle : Needle]
                                    [carrier : Carrier])]
               [#:struct Drop      ([needle : Needle])]
               [#:struct Xfer      ([needle : Needle]
                                    [target : Needle])]
               [#:struct Rack      ([racking : Integer])]
               [#:struct Nop       ()])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic command type
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

;; Instruction struct
(struct Instruction
  ([command : Command]
   [comment : String])
  #:prefab)

;; Script type
(define-type Script
  (Listof Instruction))

;; Record struct
(struct Record
  ([command : Command]
   [comment : String]
   [error : (Listof String)])
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic command accessors
;; FIXME better with compile-time checks instead of all the conditional statements

(: command-dir : Command -> Dir)
(define (command-dir self)
  (cond [(Tuck?  self) (Direction-val (Tuck-direction  self))]
        [(Knit?  self) (Direction-val (Knit-direction  self))]
        [(Split? self) (Direction-val (Split-direction self))]
        [(Miss?  self) (Direction-val (Miss-direction  self))]
        [(In?    self) (Direction-val (In-direction    self))]
        [(Out?   self) (Direction-val (Out-direction   self))]
        [else (error 'fnitout "instruction does not specify a direction")]))

(: command-carrier-position-dir : Command -> Dir)
(define (command-carrier-position-dir self)
  (cond [(Tuck?  self) (opposite-dir  (Tuck-direction  self))]
        [(Knit?  self) (opposite-dir  (Knit-direction  self))]
        [(Split? self) (opposite-dir  (Split-direction self))]
        [(Miss?  self) (opposite-dir  (Miss-direction  self))]
        [(In?    self) (opposite-dir  (In-direction    self))]
        [(Out?   self) (Direction-val (Out-direction   self))]
        [else (error 'fnitout "instruction does not specify a direction")]))

(: command-needle : Command -> Needle)
(define (command-needle self)
  (cond [(Tuck?  self) (Tuck-needle  self)]
        [(Knit?  self) (Knit-needle  self)]
        [(Split? self) (Split-needle self)]
        [(Miss?  self) (Miss-needle  self)]
        [(In?    self) (In-needle    self)]
        [(Out?   self) (Out-needle   self)]
        [(Drop?  self) (Drop-needle  self)]
        [(Xfer?  self) (Xfer-needle  self)]
        [else (error 'fnitout "instruction does not specify a needle")]))

(: command-target : Command -> Needle)
(define (command-target self)
  (cond [(Split? self) (Split-target self)]
        [(Xfer?  self) (Xfer-target  self)]
        [else (error 'fnitout "instruction does not specify a target needle")]))

(: command-length : Command -> (Option Length))
(define (command-length self)
  (cond [(Tuck?  self) (Tuck-length  self)]
        [(Knit?  self) (Knit-length  self)]
        [(Split? self) (Split-length self)]
        [else (error 'fnitout "instruction does not specify a loop length")]))

(: command-carriers : Command -> (Listof Carrier))
(define (command-carriers self)
  (cond [(Tuck?  self) (list (Yarn-carrier (Tuck-yarn self)))]
        [(Knit?  self) (map Yarn-carrier (Knit-yarns  self))]
        [(Split? self) (map Yarn-carrier (Split-yarns self))]
        [(Miss?  self) (list (Miss-carrier self))]
        [(In?    self) (list (In-carrier   self))]
        [(Out?   self) (list (Out-carrier  self))]
        [else (error 'fnitout "instruction does not specify any carriers")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; serialization methods

(: script-export (->* (Script) (Output-Port) Void))
(define (script-export self [out (current-output-port)])
  (for ([instruction (in-list self)])
    (displayln (instruction->string instruction) out)))

(: script->string : Script -> String)
(define (script->string self)
  (string-join
   (map instruction->string self)
   "\n"))

(: instruction->string : Instruction -> String)
(define (instruction->string self)
  (let ([cmd     (command->string (Instruction-command self))]
        [comment (Instruction-comment self)])
    (string-append
     cmd
     (if (zero? (string-length comment))
         ""
         (string-append
          (make-string (- 30 (string-length cmd)) #\040)
          "; "
          comment)))))

(: command->string : Command -> String)
(define (command->string self)
  (string-join
   (filter-not
    (compose zero? string-length)
    (list
     (cond [(Tuck?  self)  "tuck"]
           [(Knit?  self)  "knit"]
           [(Split? self)  "split"]
           [(Miss?  self)  "miss"]
           [(In?    self)  "in"]
           [(Out?   self)  "out"]
           [(Drop?  self)  "drop"]
           [(Xfer?  self)  "xfer"]
           [(Rack?  self)  "rack"]
           [else          ""])
     (cond [(Rack?  self) (~a (Rack-racking self))]
           [else          ""])
     (cond [(Tuck?  self) (direction->string (Tuck-direction  self))]
           [(Knit?  self) (direction->string (Knit-direction  self))]
           [(Split? self) (direction->string (Split-direction self))]
           [(Miss?  self) (direction->string (Miss-direction  self))]
           [(In?    self) (direction->string (In-direction    self))]
           [(Out?   self) (direction->string (Out-direction   self))]
           [else         ""])
     (cond [(Tuck?  self) (needle->string (Tuck-needle  self))]
           [(Knit?  self) (needle->string (Knit-needle  self))]
           [(Split? self) (needle->string (Split-needle self))]
           [(Miss?  self) (needle->string (Miss-needle  self))]
           [(In?    self) (needle->string (In-needle    self))]
           [(Out?   self) (needle->string (Out-needle   self))]
           [(Drop?  self) (needle->string (Drop-needle  self))]
           [(Xfer?  self) (needle->string (Xfer-needle  self))]
           [else         ""])
     (cond [(Split? self) (needle->string (Split-target self))]
           [(Xfer?  self) (needle->string (Xfer-target  self))]
           [else         ""])
     (cond [(Tuck?  self) (length->string (Tuck-length  self))]
           [(Knit?  self) (length->string (Knit-length  self))]
           [(Split? self) (length->string (Split-length self))]
           [else         ""])
     (cond [(Tuck?  self) (yarn->string (Tuck-yarn self))]
           [(Knit?  self) (string-join (map yarn->string (Knit-yarns  self)))]
           [(Split? self) (string-join (map yarn->string (Split-yarns self)))]
           [(Miss?  self) (carrier->string (Miss-carrier self))]
           [(In?    self) (carrier->string (In-carrier   self))]
           [(Out?   self) (carrier->string (Out-carrier  self))]
           [else         ""])))))

(: direction->string : Direction -> String)
(define (direction->string self)
  (symbol->string (Direction-val self)))

(: needle->string : Needle -> String)
(define (needle->string self)
  (string-append
   (symbol->string (Needle-bed self))
   "."
   (~a (Needle-index self))))

(: length->string : Length -> String)
(define (length->string self)
  ;(~a (Length-val self)))
  ;; for compatibility with k2f.mjs
  (let* ([val (Length-val self)]
         [val~ (if (integer? val)
                   (inexact->exact val)
                   val)])
    (~a val~)))

(: carrier->string : Carrier -> String)
(define (carrier->string self)
  (~a (Carrier-val self)))

(: yarn->string : Yarn -> String)
(define (yarn->string self)
  (string-append
   "("
   (carrier->string (Yarn-carrier self))
   ","
   (length->string (Yarn-length self))
   ")"))

;; end