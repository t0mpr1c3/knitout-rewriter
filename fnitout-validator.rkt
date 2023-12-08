#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (struct-out Record)
         (struct-out Validator)
         make-Validator
         validate
         needle-physical-position
         carrier-physical-position)

(require "fnitout-command.rkt"
         "fnitout-machine.rkt")

(define current-validity : (Parameterof (Listof String))
  (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; holds machine configuration
(struct Validator
  ([needle-count : Positive-Integer]  ;; number of needles in each bed
   [carrier-count : Positive-Integer] ;; number of yarn carriers
   [machine : MachineState]) ;; knitting machine state (mutable)
  #:transparent)

;; constructor
(: make-Validator : Positive-Integer Positive-Integer -> Validator)
(define (make-Validator needle-count carrier-count)
  (Validator
   needle-count  ;; FIXME use to create contract on Needle
   carrier-count ;; FIXME use to create contract on Carrier
   (make-MachineState needle-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: invalid : String -> Void)
(define (invalid msg)
  (displayln (format "Error in formal knitout validation: ~a" msg))
  (current-validity
   (append
    (current-validity)
    (list msg))))

;; validate formal knitout AST
(: validate : Validator Script -> (Listof Record))
(define (validate self script)
  (let ([machine (Validator-machine self)])
    (let vloop ([cmds : (Listof Instruction) script]
                [acc  : (Listof Record) null])
      (if (null? cmds)
          (reverse acc)
          (let ([cmd     (Instruction-command (car cmds))]
                [comment (Instruction-comment (car cmds))])
            (current-validity null)
            (vloop (cdr cmds)
                   (cons
                    (Record
                     (cond [(Tuck? cmd)
                            (check-carrier-positions machine cmd)
                            (let ([needle (Tuck-needle cmd)])
                              ;; increment loop count
                              (set-loops! machine needle (add1 (get-loops machine needle))))
                            (set-attachments machine cmd)
                            (set-carrier-positions machine cmd)
                            cmd]

                           [(Knit? cmd)
                            (check-carrier-positions machine cmd)
                            (let ([needle (Knit-needle cmd)]
                                  [yarns  (Knit-yarns cmd)])
                              ;; set loop count
                              (set-loops! machine needle (length yarns)))
                            (set-attachments machine cmd)
                            (set-carrier-positions machine cmd)
                            cmd]

                           [(Split? cmd)
                            (check-target machine cmd)
                            (check-carrier-positions machine cmd)
                            (let ([needle (Split-needle cmd)]
                                  [target (Split-target cmd)]
                                  [yarns  (Split-yarns cmd)])
                              ;; move loop count
                              (set-loops! machine target (+ (get-loops machine target)
                                                            (get-loops machine needle)))
                              ;; track newly created loops
                              (set-loops! machine needle (length yarns)))
                            (move-attachments machine cmd)
                            (set-attachments machine cmd)
                            (set-carrier-positions machine cmd)
                            cmd]

                           [(Drop? cmd)
                            (let ([needle (Drop-needle cmd)])
                              (if (zero? (get-loops machine needle))
                                  (invalid "needle has no loops to drop")
                                  (set-loops! machine needle 0)))
                            cmd]

                           [(Miss? cmd)
                            (check-carrier-positions machine cmd)
                            (set-carrier-positions machine cmd)
                            cmd]

                           [(In? cmd)
                            (let ([carrier-positions (MachineState-carrier-positions machine)]
                                  [c                 (Carrier-val (In-carrier cmd))])
                              (when (hash-has-key? carrier-positions c)
                                (invalid "yarn carrier is already in")))
                            (set-carrier-positions machine cmd)
                            cmd]

                           [(Out? cmd)
                            (check-carrier-positions machine cmd)
                            (let ([carrier-positions (MachineState-carrier-positions machine)]
                                  [attachments       (MachineState-attachments       machine)]
                                  [c                 (Carrier-val (Out-carrier cmd))])
                              (hash-remove! carrier-positions c)
                              (hash-remove! attachments       c))
                            cmd]

                           [(Xfer? cmd)
                            (check-target machine cmd)
                            (let ([needle (Xfer-needle cmd)]
                                  [target (Xfer-target cmd)])
                              ;; move loop count
                              (set-loops! machine target (+ (get-loops machine target)
                                                            (get-loops machine needle)))
                              (set-loops! machine needle 0))
                            (move-attachments machine cmd)
                            cmd]

                           [(Rack? cmd)
                            (let ([old (MachineState-racking machine)]
                                  [new (Rack-racking cmd)])
                              (when (= old new)
                                (invalid "redundant Rack instruction"))
                              (unless (= 1 (abs (- old new)))
                                (invalid "Rack instruction can only change the racking by +/-1"))
                              (set-MachineState-racking! machine new))
                            cmd]

                           [else cmd])
                     comment
                     (current-validity))
                    acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns physical position of needle
(: needle-physical-position : Integer Needle -> Integer)
(define (needle-physical-position racking needle)
  (let ([bed (Needle-bed needle)]
        [idx (Needle-index needle)])
    (+ idx
       (if (eq? 'f bed)
           0
           racking))))

;; returns physical position of yarn carrier
(: carrier-physical-position (->* (Integer Needle) (Dir) Integer))
(define (carrier-physical-position racking needle [dir '-])
  (+ (needle-physical-position racking needle)
     (if (eq? '+ dir)
         1
         0)))

;; check that all yarn carriers are at physical position corresponding to [n.x, dir]_r
(: check-carrier-positions : MachineState Command -> Void)
(define (check-carrier-positions machine cmd)
  (let* ([direction (command-carrier-position-dir cmd)] ;; NB command `Out` is a special case
         [needle    (command-needle cmd)]
         [carriers  (command-carriers cmd)]
         [racking   (MachineState-racking machine)]
         [expected  (carrier-physical-position racking needle direction)]
         [positions (MachineState-carrier-positions machine)])
    (for ([c (in-list carriers)])
      (let ([y (Carrier-val c)])
        (if (not (hash-has-key? positions y))
            (invalid (format "yarn carrier ~a is not in action" y))
            (let ([actual (hash-ref positions y)])
              (unless (= expected actual)
                (invalid (format "expected yarn carrier ~a at position ~a, but it is at ~a" y expected actual)))))))))

;; check that source and target needles are aligned
(: check-target : MachineState Command -> Void)
(define (check-target machine cmd)
  (let* ([needle  (command-needle cmd)]
         [target  (command-target cmd)]
         [racking (MachineState-racking machine)])
    (when (eq? (Needle-bed needle)
               (Needle-bed target))
      (error 'fnitout "needle and target are on same bed"))
    (unless (= (carrier-physical-position racking needle)
               (carrier-physical-position racking target))
      (invalid "needle and target are not aligned"))))

;; move all attached loops from source needle to target
(: move-attachments : MachineState Command -> Void)
(define (move-attachments machine cmd)
  (let ([attachments (MachineState-attachments machine)]
        [needle (command-needle cmd)]
        [target (command-target cmd)])
    (for ([y (in-hash-keys attachments)])
      (when (equal? needle
                    (hash-ref attachments y))
        (hash-set! attachments y target)))))

;; set attachments at needle specified by instruction
;; NB. JL also sets direction of attachment, but it is not used
(: set-attachments : MachineState Command -> Void)
(define (set-attachments machine cmd)
  (let* ([attachments (MachineState-attachments machine)]
         [needle (command-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)])
    (for ([y (in-list yarns)])
      (hash-set! attachments y needle))))

;; set physical position of yarn carriers
(: set-carrier-positions : MachineState Command -> Void)
(define (set-carrier-positions machine cmd)
  (let* ([carrier-positions (MachineState-carrier-positions machine)]
         [dir (command-dir cmd)]
         [needle (command-needle cmd)]
         [carriers (command-carriers cmd)]
         [yarns (map Carrier-val carriers)]
         [racking (MachineState-racking machine)])
    (for ([y (in-list yarns)])
      (hash-set! carrier-positions y (carrier-physical-position racking needle dir)))))

;; end
