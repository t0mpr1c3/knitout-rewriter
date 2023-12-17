#lang typed/racket

;; https://doi.org/10.1145/3592449

(provide merge-rack
         merge-miss
         squish
         slide
         conjugate
         assign-dir-aux
         sort-xfer
         sort-xfer-aux         
         sort-drop!
         passes-sort-out
         sort-out!
         passes-sort-in
         sort-in
         sort-in-aux)

(require threading)
(require "fnitout-machine.rkt"
         "fnitout-command.rkt"
         "fnitout-validator.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; opposite, consecutive Rack commands cancel
(: merge-rack : OpList Natural -> OpList)
(define (merge-rack ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'merge-rack "merge not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Rack? op1)
                 (Rack? op2)
                 (zero? (+ (Rack-racking op1)
                           (Rack-racking op2)))) ;; equal and opposite
      (error 'merge-rack "merge not possible at position ~a" pos))
    ;; eliminate both
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 2
;; when Miss at needle N and carrier C in one direction
;; is followed by Miss at needle N and carrier C in opposite direction,
;; both are eliminated
(: merge-miss : OpList Natural -> OpList)
(define (merge-miss ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'merge-miss "merge not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Miss? op1)
                 (Miss? op2)
                 (equal? (Miss-needle op1)
                         (Miss-needle op2))           ;; same needle
                 (equal? (Miss-carrier op1)
                         (Miss-carrier op2))          ;; same carrier
                 (not (equal? (Miss-direction op1)
                              (Miss-direction op2)))) ;; different directions
      (error 'merge-miss "merge not possible at position ~a" pos))
    ;; eliminate both
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 3
;; eliminates the first of consecutive, opposite Xfers
(: squish : OpList Natural -> OpList)
(define (squish ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'squish "squish not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Xfer? op1)
                 (Xfer? op2)
                 (equal? (Xfer-needle op1)
                         (Xfer-target op2))  ;; same needle
                 (equal? (Xfer-target op1)
                         (Xfer-needle op2))) ;; same needle
      (error 'squish "squish not possible at position ~a" pos))
    ;; eliminate first Xfer
    (append
     (take ops pos)
     (drop ops (+ 1 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 4
;; changes the needle location where Tuck is performed
(: slide : OpList Natural -> OpList)
(define (slide ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'slide "slide not possible at position ~a" pos))
  (let ([op1 (list-ref ops pos)]
        [op2 (list-ref ops (add1 pos))])
    (unless (and (Tuck? op1)
                 (Xfer? op2) ;; Tuck then Xfer
                 (equal? (Tuck-needle op1)
                         (Xfer-needle op2))) ;; same needle
      (error 'slide "slide not possible at position ~a" pos))
    ;; change location of Tuck
    (append
     (take ops pos)
     (list
      (struct-copy Tuck op1
                   [needle (Xfer-target op2)]))
     (drop ops (+ 1 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rewrite rule 5
;; changes the needle location where Knit or Tuck is performed
;; Table 2
(: conjugate : MachineState OpList Natural Dir -> OpList)
(define (conjugate machine ops pos dir)
  (when (> (add1 pos) (length ops))
    (error 'conjugate "eliminate not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]         
         [ops0 (update! machine0 ops pos)]
         [op (car ops0)]) ;; (list-ref ops pos)
    (unless (or (Knit? op)
                (Tuck? op))
      (error 'conjugate "only Knit and Tuck operations can be conjugated"))
    (let* ([r (MachineState-racking machine0)]
           [d (op-dir op)]
           [n (op-needle op)]
           [cs (command-carriers op)]
           [b (Needle-bed n)]
           [x (Needle-index n)]
           [x-1 (- x 1)]
           [x+1 (+ x 1)]
           [r-1 (- r 1)]
           [r+1 (+ r 1)]
           [n~ (Needle b (if (eq? '- dir) x-1 x+1))]
           [op~ (if (Knit? op)
                    (struct-copy Knit op [needle n~])
                    (struct-copy Tuck op [needle n~]))]
           [conjugated    
            (cond [(and (eq? 'f b)
                        (eq? '+ d)               
                        (eq? '- dir)) ;; Left
                   `(,@(MISS '- (Needle 'f x-1) cs)
                     ;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,op~
                     ,@(MISS '+ (Needle 'f x) cs)
                     ;,(SHIFT n~ r-1 1)
                     ,(MOVE n~ r 1))]

                  [(and (eq? 'f b)
                        (eq? '+ d)
                        (eq? '+ dir)) ;; Right
                   `(;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,@(MISS '+ (Needle 'f x) cs)
                     ,op~
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1)
                     ,@(MISS '- (Needle 'f x+1) cs))]

                  [(and (eq? 'f b)
                        (eq? '- d)
                        (eq? '- dir)) ;; Left
                   `(;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,@(MISS '- (Needle 'f x) cs)
                     ,op~
                     ;,(SHIFT n~ r-1 +1)
                     ,(MOVE n~ r +1)
                     ,@(MISS '+ (Needle 'f x-1) cs))]

                  [(and (eq? 'f b)
                        (eq? '- d)
                        (eq? '+ dir)) ;; Right
                   `(,@(MISS '+ (Needle 'f x+1) cs)
                     ;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,op~
                     ,@(MISS '- (Needle 'f x) cs)
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1))]

                  [(and (eq? 'b b)
                        (eq? '- dir)) ;; Left
                   `(;,(SHIFT n r -1)
                     ,(MOVE n r -1)
                     ,op~
                     ;,(SHIFT n~ r-1 +1)
                     ,(MOVE n~ r +1))]

                  [(and (eq? 'b b)
                        (eq? '+ dir)) ;; Right
                   `(;,(SHIFT n r +1)
                     ,(MOVE n r +1)
                     ,op~
                     ;,(SHIFT n~ r+1 -1)
                     ,(MOVE n~ r -1))]

                  [else
                   (error 'conjugate "conjugate not possible at position ~a" pos)])])
      (append
       (take ops pos)
       conjugated
       (drop ops (add1 pos))))))

(: MISS : Dir Needle (Listof Carrier) -> (Listof Command))
(define (MISS d n cs)
  (for/list ([c (in-list cs)])
    (Miss (Direction d) n c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra rule:
;; eliminate operations that do nothing.
;; * Nop
;; * Xfer when there are no loops on the source needle
;; * Rack when the racking is the current value

(: eliminate : MachineState OpList Natural -> OpList)
(define (eliminate machine ops pos)
  (unless (< (add1 pos) (length ops))
    (error 'eliminate "eliminate not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]
         [ops0 (update! machine0 ops pos)]
         [op (car ops0)]) ;; (list-ref ops pos)
    (if (or (Nop? op)
            (and (Rack? op)
                 (= (Rack-racking op)
                    (MachineState-racking machine0)))
            (and (Xfer? op)
                 (zero? (get-loops machine0 (op-needle op))))
            (and (MOVE? op)
                 (zero? (get-loops machine0 (op-needle op)))
                 (zero? (get-loops machine0 (op-target op)))))
        (append
         (take ops pos)
         (drop ops (add1 pos)))
        (error 'eliminate "eliminate not possible at position ~a" pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra rule
;; cancel consecutive, opposite Xfer/MOVE when there are no loops on the target
(: cancel : MachineState OpList Natural -> OpList)
(define (cancel machine ops pos)
  (when (> (+ 2 pos) (length ops))
    (error 'cancel "cancel not possible at position ~a" pos))
  (let* ([machine0 (machine-copy machine)]
         [ops0 (update! machine0 ops pos)]
         [op1 (first  ops0)]  ;; (list-ref ops0 pos)
         [op2 (second ops0)]) ;; (list-ref ops0 (add1 pos))
    (unless (and (or (and (Xfer? op1)
                          (Xfer? op2))
                     (and (MOVE? op1)
                          (MOVE? op2)))
                 (equal? (op-needle op1)
                         (op-target op2)) ;; same needle
                 (equal? (op-target op1)
                         (op-needle op2)) ;; same needle
                 (zero? (get-loops machine0 (op-target op1)))) ;; no loops on target
      (error 'cancel "cancel not possible at position ~a" pos))
    ;; cancel both operations
    (append
     (take ops pos)
     (drop ops (+ 2 pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; group of instructions in regular sequence across needle bed
;; FIXME pattern matching needs to be a lot more sophisticated than this

(struct Group
  ([op    : Operation]
   [inc   : Integer]
   [count : Positive-Integer])
  #:prefab)

;; constructor
(: make-group : Operation Integer -> Group)
(define (make-group op inc)
  (unless (op-needle? op)
    (error 'make-group "operation ~a cannot form a group" op))
  (Group op inc 1))

(: group-append : Group -> Group)
(define (group-append self)
  (struct-copy Group self
               [inc (add1 (Group-inc self))]))

(: group-ref : Group Natural -> Operation)
(define (group-ref self index)
  (unless (< index (Group-count self))
    (error 'group-ref "index too large for group\nindex: ~a\ncount: ~a" index count))
  (group-member self (* (Group-inc   self)
                        index)))

(: group-next : Group -> Operation)
(define (group-next self)
  (group-member self (* (Group-inc   self)
                        (Group-count self))))

(: group-member : Group Integer -> Operation)
(define (group-member self inc)
  (let* ([op (Group-op self)]
         [n (op-needle op)]
         [x (Needle-index n)]
         [n~ (struct-copy Needle n
                          [index (+ x inc)])])
    (if (op-target? op)
        (let* ([t (op-target op)]
               [y (Needle-index t)]
               [t~ (struct-copy Needle t
                                [index (+ y inc)])])
          (op-copy op
                   #:needle n~
                   #:target t~))
        (op-copy op
                 #:needle n~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; end
