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

;; Command type
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
(define Command? (make-predicate Command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACK operation
;; sequence of Rack commands
(struct RACK
  ([r : Integer]
   [j : Integer])
  #:prefab)

(: RACK-racking : RACK -> Integer)
(define (RACK-racking self)
  (+ (RACK-r self)
     (RACK-j self)))

(: RACK-cmds : RACK -> (Listof Command))
(define (RACK-cmds self)
  (let ([r (RACK-r self)]
        [j (RACK-j self)])
    (let loop ([i : Integer 0]
               [a : (Listof Command) null])
      (if (= i j)
          (reverse a)
          (let ([i~ (+ i (sign (- j i)))])
            (loop i~
                  (cons (Rack (+ r i~))
                        a)))))))

(: sign : Real -> Integer)
(define (sign x)
  (if (positive? x)
      +1
      (if (negative? x)
          -1
          0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SHIFT operation
;; moves loops from one needle to another needle on the same bed
(struct SHIFT
  ([needle : Needle]
   [target : Needle]
   [r : Integer])
  #:prefab)

(: SHIFT-j : SHIFT -> Integer)
(define (SHIFT-j self)
  (let* ([n (SHIFT-needle self)]
         [t (SHIFT-target self)]
         [ni (Needle-index n)]
         [ti (Needle-index t)])
    (- ti ni)))

(: SHIFT-racking : SHIFT -> Integer)
(define (SHIFT-racking self)
  (let* ([n (SHIFT-needle self)]
         [r (SHIFT-r self)]
         [j (SHIFT-j self)]
         [nb (Needle-bed n)])
    (if (eq? 'f nb)
        (+ r j)
        (- r j))))

(: SHIFT-cmds : SHIFT -> (Listof Command))
(define (SHIFT-cmds self)
  (let* ([n (SHIFT-needle self)]
         [t (SHIFT-target self)]
         [r (SHIFT-r self)]
         [nb (Needle-bed n)]
         [ni (Needle-index n)]
         [tb (Needle-bed t)]
         [ti (Needle-index t)]
         [j (- ti ni)])
    (assert (eq? nb tb))
    (if (eq? 'f nb)
        `(,(Xfer n (Needle 'b (- ni r)))
          ,@(RACK-cmds (RACK r j))
          ,(Xfer (Needle 'b (- ni r)) t))
        `(,(Xfer n (Needle 'f (+ ni r)))
          ,@(RACK-cmds (RACK r (- j)))
          ,(Xfer (Needle 'f (+ ni r)) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MOVE operation
;; SHIFT followed by RACK to reset racking
(struct MOVE
  ([needle : Needle]
   [target : Needle]
   [r : Integer])
  #:prefab)

(: MOVE-racking : MOVE -> Integer)
(define (MOVE-racking self)
  (MOVE-r self))

(: MOVE-j : MOVE -> Integer)
(define (MOVE-j self)
  (let* ([n (MOVE-needle self)]
         [t (MOVE-target self)]
         [r (MOVE-r self)]
         [s (SHIFT n t r)])
    (SHIFT-j s)))

(: MOVE-cmds : MOVE -> (Listof Command))
(define (MOVE-cmds self)
  (let* ([n (MOVE-needle self)]
         [t (MOVE-target self)]
         [r (MOVE-r self)]
         [s (SHIFT n t r)]
         [j (SHIFT-j s)]
         [nb (Needle-bed n)])
    (append
     (SHIFT-cmds s)
     (if (eq? 'f nb)
         (RACK-cmds (RACK (+ r j) (- j)))
         (RACK-cmds (RACK (- r j) j))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operation type
(define-type Operation
  (U Command
     RACK
     SHIFT
     MOVE))

;; Instruction struct
(struct Instruction
  ([op : Operation]
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

;; generic operation accessors
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

(: op-needle : Operation -> Needle)
(define (op-needle self)
  (cond [(Tuck?  self) (Tuck-needle  self)]
        [(Knit?  self) (Knit-needle  self)]
        [(Split? self) (Split-needle self)]
        [(Miss?  self) (Miss-needle  self)]
        [(In?    self) (In-needle    self)]
        [(Out?   self) (Out-needle   self)]
        [(Drop?  self) (Drop-needle  self)]
        [(Xfer?  self) (Xfer-needle  self)]
        [(SHIFT? self) (SHIFT-needle self)]
        [(MOVE?  self) (MOVE-needle  self)]
        [else (error 'fnitout "instruction does not specify a needle")]))

(: op-target : Operation -> Needle)
(define (op-target self)
  (cond [(Split? self) (Split-target self)]
        [(Xfer?  self) (Xfer-target  self)]
        [(SHIFT? self) (SHIFT-target self)]
        [(MOVE?  self) (MOVE-target  self)]
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

(: op-racking : Operation -> Integer)
(define (op-racking self)
  (cond [(Rack?  self) (Rack-racking  self)]
        [(RACK?  self) (RACK-racking  self)]
        [(SHIFT? self) (SHIFT-racking self)]
        [(MOVE?  self) (MOVE-racking  self)]
        [else (error 'fnitout "instruction does not specify racking")]))

(: op-cmds : Operation -> (Listof Command))
(define (op-cmds self)
  (cond [(Command? self) (list self)]
        [(RACK?    self) (RACK-cmds  self)]
        [(SHIFT?   self) (SHIFT-cmds self)]
        [(MOVE?    self) (MOVE-cmds  self)]
        [else (error 'fnitout "unrecognized operation ~a" self)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: script-expand : Script -> (Listof (Pairof Command String)))
(define (script-expand self)
  (apply append (map instruction-expand self)))

(: instruction-expand : Instruction -> (Listof (Pairof Command String)))
(define (instruction-expand self)
  (let* ([cmds (op-cmds (Instruction-op self))]
         [comment (Instruction-comment self)]
         [cmd1 (car cmds)])
    (append
     (list (cons cmd1 comment))
     (if (= 1 (length cmds))
         null
         (map (λ ([cmd : Command])
                (cons cmd ""))
              (cdr cmds))))))

;; end