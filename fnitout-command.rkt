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
(define Dir? (make-predicate Dir))

(: opposite : Dir -> Dir)
(define (opposite dir)
  (if (eq? '+ dir) '- '+))

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
  ([n : Needle]
   [r : Integer]
   [j : Integer])
  #:prefab)

(: SHIFT-needle : SHIFT -> Needle)
(define (SHIFT-needle self)
  (SHIFT-n self))

(: SHIFT-target : SHIFT -> Needle)
(define (SHIFT-target self)
  (let* ([n (SHIFT-n self)]
         [j (SHIFT-j self)]
         [b (Needle-bed n)]
         [x (Needle-index n)])
    (Needle b (+ x j))))

(: SHIFT-racking : SHIFT -> Integer)
(define (SHIFT-racking self)
  (let* ([n (SHIFT-n self)]
         [r (SHIFT-r self)]
         [j (SHIFT-j self)]
         [b (Needle-bed n)])
    (if (eq? 'f b)
        (+ r j)
        (- r j))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MOVE operation
;; SHIFT followed by RACK to reset racking
(struct MOVE
  ([n : Needle]
   [r : Integer]
   [j : Integer])
  #:prefab)

(: MOVE-needle : MOVE -> Needle)
(define (MOVE-needle self)
  (MOVE-n self))

(: MOVE-target : MOVE -> Needle)
(define (MOVE-target self)
  (let* ([n (MOVE-n self)]
         [j (MOVE-j self)]
         [b (Needle-bed n)]
         [x (Needle-index n)])
    (Needle b (+ x j))))

(: MOVE-racking : MOVE -> Integer)
(define (MOVE-racking self)
  (MOVE-r self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Operation type
(define-type Operation
  (U Command
     RACK
     SHIFT
     MOVE))

(: op-copy (->* (Operation) (#:needle (Option Needle)
                             #:target (Option Needle)) Operation))
(define (op-copy op #:needle [n #f] #:target [t #f])
  (when (and (not (false? n))
             (not (op-needle? op)))
    (error 'op-copy "cannot specify needle for operation ~a" op))
  (when (and (not (false? t))
             (not (op-target? op)))
    (error 'op-copy "cannot specify target for operation ~a" op))
  (let* ([n~ (or n (op-needle op))]
         [t~ (or t (op-target op))]
         [j (- (Needle-index t~)
               (Needle-index n~))])
    (cond [(Tuck?  op) (struct-copy Tuck op
                                    [needle n~])]
          [(Knit?  op) (struct-copy Knit op
                                    [needle n~])]
          [(Split? op) (struct-copy Split op
                                    [needle n~]
                                    [target t~])]
          [(Miss?  op) (struct-copy Miss op
                                    [needle n~])]
          [(In?    op) (struct-copy In op
                                    [needle n~])]
          [(Out?   op) (struct-copy Out op
                                    [needle n~])]
          [(Drop?  op) (struct-copy Drop op
                                    [needle n~])]
          [(Xfer?  op) (struct-copy Xfer op
                                    [needle n~]
                                    [target t~])]
          [(SHIFT? op) (struct-copy SHIFT op
                                    [n n~]
                                    [j j])]
          [(MOVE?  op) (struct-copy MOVE op
                                    [n n~]
                                    [j j])]
          [else (error 'op-copy "fallthrough error")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(: op-dir? : Operation -> Boolean)
(define (op-dir? self)
  (or (Tuck?  self)
      (Knit?  self)
      (Split? self)
      (Miss?  self)
      (In?    self)
      (Out?   self)))

(: op-dir : Operation -> Dir)
(define (op-dir self)
  (cond [(Tuck?  self) (Direction-val (Tuck-direction  self))]
        [(Knit?  self) (Direction-val (Knit-direction  self))]
        [(Split? self) (Direction-val (Split-direction self))]
        [(Miss?  self) (Direction-val (Miss-direction  self))]
        [(In?    self) (Direction-val (In-direction    self))]
        [(Out?   self) (Direction-val (Out-direction   self))]
        [else (error 'fnitout "instruction does not specify a direction")]))

;; logical position of carrier prior to operation
;; Fig. 11
(: command-carrier-position-dir : Command -> Dir)
(define (command-carrier-position-dir self)
  (cond [(Tuck?  self) (opposite-dir  (Tuck-direction  self))]
        [(Knit?  self) (opposite-dir  (Knit-direction  self))]
        [(Split? self) (opposite-dir  (Split-direction self))]
        [(Miss?  self) (opposite-dir  (Miss-direction  self))]
        [(In?    self) (opposite-dir  (In-direction    self))]
        [(Out?   self) (Direction-val (Out-direction   self))]
        [else (error 'fnitout "instruction does not specify a direction")]))

(: op-needle? : Operation -> Boolean)
(define (op-needle? self)
  (or (Tuck?  self)
      (Knit?  self)
      (Split? self)
      (Miss?  self)
      (In?    self)
      (Out?   self)
      (Drop?  self)
      (Xfer?  self)
      (SHIFT? self)
      (MOVE?  self)))

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

(: op-target? : Operation -> Boolean)
(define (op-target? self)
  (or (Split? self)
      (Xfer?  self)
      (SHIFT? self)
      (MOVE?  self)))

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

(: command-carriers? : Command -> Boolean)
(define (command-carriers? self)
  (or (Tuck?  self)
      (Knit?  self)
      (Split? self)
      (Miss?  self)
      (In?    self)
      (Out?   self)
      (Drop?  self)
      (Xfer?  self)))

(: command-carriers : Command -> (Listof Carrier))
(define (command-carriers self)
  (cond [(Tuck?  self) (list (Yarn-carrier (Tuck-yarn self)))]
        [(Knit?  self) (map Yarn-carrier (Knit-yarns  self))]
        [(Split? self) (map Yarn-carrier (Split-yarns self))]
        [(Miss?  self) (list (Miss-carrier self))]
        [(In?    self) (list (In-carrier   self))]
        [(Out?   self) (list (Out-carrier  self))]
        [(Drop?  self) null]
        [(Xfer?  self) null]
        [else (error 'fnitout "instruction does not specify any carriers")]))

(: op-racking : Operation -> Integer)
(define (op-racking self)
  (cond [(Rack?  self) (Rack-racking  self)]
        [(RACK?  self) (RACK-racking  self)]
        [(SHIFT? self) (SHIFT-racking self)]
        [(MOVE?  self) (MOVE-racking  self)]
        [else (error 'fnitout "instruction does not specify racking")]))

(: op->cmds : Operation -> (Listof Command))
(define (op->cmds self)
  (cond [(Command? self) (list self)]
        [(RACK?    self) (RACK->cmds  self)]
        [(SHIFT?   self) (SHIFT->cmds self)]
        [(MOVE?    self) (MOVE->cmds  self)]
        [else (error 'fnitout "unrecognized operation ~a" self)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: script-expand : Script -> (Listof (Pairof Command String)))
(define (script-expand self)
  (apply append (map instruction-expand self)))

(: instruction-expand : Instruction -> (Listof (Pairof Command String)))
(define (instruction-expand self)
  (let* ([cmds (op->cmds (Instruction-op self))]
         [comment (Instruction-comment self)]
         [cmd1 (car cmds)])
    (append
     (list (cons cmd1 comment))
     (if (= 1 (length cmds))
         null
         (map (λ ([cmd : Command])
                (cons cmd ""))
              (cdr cmds))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions to rewrite operation as list of commands

(: RACK->cmds : RACK -> (Listof Command))
(define (RACK->cmds self)
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

(: SHIFT->cmds : SHIFT -> (Listof Command))
(define (SHIFT->cmds self)
  (let* ([n (SHIFT-n self)]
         [r (SHIFT-r self)]
         [j (SHIFT-j self)]
         [b (Needle-bed n)]
         [x (Needle-index n)]
         [x+j (+ x j)]
         [x+r (+ x r)]
         [x-r (- x r)])
    (if (eq? 'f b)
        `(,(Xfer n (Needle 'b x-r))
          ,@(RACK->cmds (RACK r j))
          ,(Xfer (Needle 'b x-r) (Needle 'f x+j)))
        `(,(Xfer n (Needle 'f x+r))
          ,@(RACK->cmds (RACK r (- j)))
          ,(Xfer (Needle 'f x+r) (Needle 'b x+j))))))

(: MOVE->cmds : MOVE -> (Listof Command))
(define (MOVE->cmds self)
  (let* ([n (MOVE-n self)]
         [r (MOVE-r self)]
         [j (MOVE-j self)]
         [b (Needle-bed n)])
    (append
     (SHIFT->cmds (SHIFT n r j))
     (if (eq? 'f b)
         (RACK->cmds (RACK (+ r j) (- j)))
         (RACK->cmds (RACK (- r j) j))))))

#|
;; or equivalently:
(if (eq? 'f b)
    (append
     (RACK->cmds (RACK (+ r (- j))))
     (SHIFT->cmds (SHIFT n (- r j) j)))
    (append
     (RACK->cmds (RACK (+ r j)))
     (SHIFT->cmds (SHIFT n (+ r j) j))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions to rewrite list of commands as operation

(: cmds->RACK : Integer Integer (Listof Operation) Natural -> (Listof Operation))
(define (cmds->RACK r j ops pos)
  (when (zero? j)
    (error 'fnitout "cmds->RACK ~a 0 is redundant" r))
  (let ([len (length ops)])
    (when (and (< pos len)
               (equal? (list-ref ops pos)
                       (RACK r j)))
      ops)
    (unless (< (+ pos (abs j)) len)
      (error 'fnitout "cmds->RACK ~a ~a not possible at position ~a" r j pos))
    (for ([i (in-range 0 j (sign j))])
      (let ([op (list-ref ops (+ pos i))])
        (unless (and (Rack? op)
                     (= (+ r i)
                        (Rack-racking op)))
          (error 'fnitout "cmds->RACK ~a ~a not possible at position ~a" r j pos))))
    (append
     (take ops pos)
     (list (RACK r j))
     (drop ops (+ pos (abs j))))))

(: cmds->SHIFT : Needle Integer Integer (Listof Operation) Natural -> (Listof Operation))
(define (cmds->SHIFT n r j ops pos)
  (when (and (< pos (length ops))
             (equal? (list-ref ops pos)
                     (SHIFT n r j)))
    ops)
  (let* ([b (Needle-bed n)]
         [ops~ (if (eq? 'f b)
                   (cmds->RACK r     j ops (add1 pos))
                   (cmds->RACK r (- j) ops (add1 pos)))])
    (unless (< (+ 2 pos) (length ops~))
      (error 'fnitout "cmds->SHIFT ~a ~a not possible at position ~a" n r j pos))
    (let ([op1 (list-ref ops~ pos)]
          [op2 (list-ref ops~ (+ 2 pos))])
      (unless (and (Xfer? op1)
                   (Xfer? op2))
        (error 'fnitout "cmds->SHIFT ~a ~a not possible at position ~a" n r j pos))
      (let ([n1 (op-needle op1)]
            [t1 (op-target op1)]
            [n2 (op-needle op2)]
            [t2 (op-target op2)]
            [x  (Needle-index n)])
        (unless (and (equal? n1 n)
                     (if (eq? 'f b)
                         (and (equal? t1 (Needle 'b (- x r)))
                              (equal? n2 (Needle 'b (- x r)))
                              (equal? t2 (Needle 'f (+ x j))))
                         (and (equal? t1 (Needle 'f (+ x r)))
                              (equal? n2 (Needle 'f (+ x r)))
                              (equal? t2 (Needle 'b (+ x j))))))
          (error 'fnitout "cmds->SHIFT ~a ~a not possible at position ~a" n r j pos))
        (append
         (take ops~ pos)
         (list (SHIFT n r j))
         (drop ops (+ 2 pos)))))))

;;  MOVE can also be written with the RACK operation first
(: cmds->MOVE : Needle Integer Integer (Listof Operation) Natural -> (Listof Operation))
(define (cmds->MOVE n r j ops pos)
  (when (and (< pos  (length ops))
             (equal? (list-ref ops pos)
                     (MOVE n r j)))
    ops)
  (let* ([b (Needle-bed n)]
         [ops1 (cmds->SHIFT n r j ops pos)]
         [ops2 (if (eq? 'f b)
                   (cmds->RACK (+ r j) (- j) ops1 (add1 pos))
                   (cmds->RACK (- r j) j     ops1 (add1 pos)))])
    (append
     (take ops2 pos)
     (list (MOVE n r j))
     (drop ops (+ 2 pos)))))

;; end