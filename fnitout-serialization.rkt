#lang typed/racket

(provide script-export
         script->string)

(require threading)
(require "fnitout-command.rkt")

;; serialization methods

(define-type Representation
  (U 'fnitout 'knitout))

(: script-export (->* (Script) (Representation Output-Port) Void))
(define (script-export self [as 'fnitout] [out (current-output-port)])
  (for ([instruction (in-list self)])
    (displayln (instruction->string instruction as) out)))

(: script->string (->* (Script) (Representation) String))
(define (script->string self [as 'fnitout])
  (string-join
   (map (λ ([x : Instruction])
          (instruction->string x as))
        self)
   "\n"))

(: instruction->string (->* (Instruction) (Representation) String))
(define (instruction->string self [as 'fnitout])
  (string-join
   (map (λ ([x : (Pairof Command String)])
          (let ([command (command->string (car x) as)]
                [comment (cdr x)])
            (string-append
             command
             (if (zero? (string-length comment))
                 ""
                 (string-append
                  (if (and (eq? as 'knitout)
                           (zero? (string-length command))
                           (string-prefix? comment ";")) ;; comment in original knitout
                      ""
                      (string-append
                       (make-string (max 0 (- 30 (string-length command))) #\040)
                       "; "))
                  comment)))))
        (instruction-expand self))
   "\n"))

(: command->string(->* (Command) (Representation) String))
(define (command->string self [as 'fnitout])
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
           [else ""])
     (cond [(Rack?  self) (~a (Rack-racking self))]
           [else ""])
     (cond [(Tuck?  self) (direction->string (Tuck-direction  self))]
           [(Knit?  self) (direction->string (Knit-direction  self))]
           [(Split? self) (direction->string (Split-direction self))]
           [(Miss?  self) (direction->string (Miss-direction  self))]
           [(eq? as 'knitout) ""]
           [(In?    self) (direction->string (In-direction    self))]
           [(Out?   self) (direction->string (Out-direction   self))]
           [else ""])
     (cond [(Tuck?  self) (needle->string (Tuck-needle  self) as)]
           [(Knit?  self) (needle->string (Knit-needle  self) as)]
           [(Split? self) (needle->string (Split-needle self) as)]
           [(Miss?  self) (needle->string (Miss-needle  self) as)]
           [(Drop?  self) (needle->string (Drop-needle  self) as)]
           [(Xfer?  self) (needle->string (Xfer-needle  self) as)]
           [(eq? as 'knitout) ""]
           [(In?    self) (needle->string (In-needle    self) as)]
           [(Out?   self) (needle->string (Out-needle   self) as)]
           [else ""])
     (cond [(Split? self) (needle->string (Split-target self) as)]
           [(Xfer?  self) (needle->string (Xfer-target  self) as)]
           [else ""])
     (cond [(Tuck?  self) (length->string (Tuck-length  self) as)]
           [(Knit?  self) (length->string (Knit-length  self) as)]
           [(Split? self) (length->string (Split-length self) as)]
           [else ""])
     (cond [(Tuck?  self) (yarn->string  (Tuck-yarn   self) as)]
           [(Knit?  self) (yarns->string (Knit-yarns  self) as)]
           [(Split? self) (yarns->string (Split-yarns self) as)]
           [(Miss?  self) (carrier->string (Miss-carrier self))]
           [(In?    self) (carrier->string (In-carrier   self))]
           [(Out?   self) (carrier->string (Out-carrier  self))]
           [else ""])))))

(: direction->string : Direction -> String)
(define (direction->string self)
  (symbol->string (Direction-val self)))

(: needle->string (->* (Needle) (Representation) String))
(define (needle->string self [as 'fnitout])
  (string-append
   (symbol->string (Needle-bed self))
   (if (eq? as 'fnitout) "." "")
   (~a (Needle-index self))))

(: length->string (->* (Length) (Representation) String))
(define (length->string self [as 'fnitout])
  (if (eq? as 'knitout)
      ""
      ;(~a (Length-val self))) ;; print as float
      ;; for compatibility with k2f.mjs
      (let* ([val (Length-val self)]
             [val~ (if (integer? val)
                       (inexact->exact val)
                       val)])
        (~a val~))))

(: carrier->string : Carrier -> String)
(define (carrier->string self)
  (~a (Carrier-val self)))

(: yarn->string (->* (Yarn) (Representation) String))
(define (yarn->string self [as 'fnitout])
  (if (eq? as 'knitout)
      (carrier->string (Yarn-carrier self))
      (string-append "("
                     (carrier->string (Yarn-carrier self))
                     ","
                     (length->string (Yarn-length self))
                     ")")))

(: yarns->string (->* ((Listof Yarn)) (Representation) String))
(define (yarns->string self [as 'fnitout])
  (apply string-append
         (map (λ ([x : Yarn])
                (yarn->string x as))
              self)))