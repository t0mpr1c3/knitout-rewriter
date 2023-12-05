#lang typed/racket
;; FIXME convert to untyped after testing

;; https://doi.org/10.1145/3592449

(provide (all-defined-out))

(require "fnitout-command.rkt"
         "fnitout-machine.rkt")

(require/typed "fnitout-parser.rkt"
               [fnitout-parse (String -> (Listof Command))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; holds parsed script and machine configuration
(struct Validater
  ([script : (Listof Command)] ;; parsed script
   [needle-count : Positive-Integer]  ;; number of needles in each bed
   [carrier-count : Positive-Integer] ;; number of yarn carriers
   [machine : MachineState]) ;; knitting machine state (mutable)
  #:transparent)

;; constructor
(: make-Validater : Positive-Integer Positive-Integer String -> Validater)
(define (make-Validater needle-count carrier-count str)
  (Validater
   (fnitout-parse str)
   needle-count  ;; FIXME use to create contract on Needle
   carrier-count ;; FIXME use to create contract on Carrier
   (make-MachineState needle-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; validate formal knitout AST
;; FIXME accumulate and report all validation errors
;;       instead of halting validation on first error
(: validate : Validater -> Void)
(define (validate self)
  (let ([machine (Validater-machine self)])
    (for ([cmd (in-list (Validater-script self))])
      (begin

        (when (Tuck? cmd)
          (check-carrier-positions machine cmd)
          (let ([needle (OpN-needle cmd)])
            ;; increment loop count
            (set-loops! machine needle (add1 (get-loops machine needle))))
          (set-attachments machine cmd)
          (set-carrier-positions machine cmd))

        (when (Knit? cmd)
          (check-carrier-positions machine cmd)
          (let ([needle (OpN-needle cmd)]
                [yarns  (OpNDLY-yarns cmd)])
            ;; set loop count
            (set-loops! machine needle (length yarns)))
          (set-attachments machine cmd)
          (set-carrier-positions machine cmd))

        (when (Split? cmd)
          (check-target machine cmd)
          (check-carrier-positions machine cmd)
          (let ([needle (OpN-needle cmd)]
                [target (Split-target cmd)]
                [yarns  (OpNDLY-yarns cmd)])
            ;; move loop count
            (set-loops! machine target (+ (get-loops machine target)
                                          (get-loops machine needle)))
            ;; track newly created loops
            (set-loops! machine needle (length yarns)))
          (move-attachments machine cmd)
          (set-attachments machine cmd)
          (set-carrier-positions machine cmd))

        (when (Drop? cmd)
          (let ([needle (OpN-needle cmd)])
            (if (zero? (get-loops machine needle))
                (error 'fnitout "validating ~a:\nneedle has no loops to drop" cmd)
                (set-loops! machine needle 0))))

        (when (Miss? cmd)
          (check-carrier-positions machine cmd)
          (set-carrier-positions machine cmd))

        (when (In? cmd)
          (let ([carrier-positions (MachineState-carrier-positions machine)]
                [c                 (Carrier-val (OpNDC-carrier cmd))])
            (when (hash-has-key? carrier-positions c)
              (error 'fnitout "validating ~a:\nyarn carrier is already in" cmd)))
          (set-carrier-positions machine cmd))

        (when (Out? cmd)
          (check-carrier-positions machine cmd)
          (let ([carrier-positions (MachineState-carrier-positions machine)]
                [attachments       (MachineState-attachments       machine)]
                [c                 (Carrier-val (OpNDC-carrier cmd))])
            (hash-remove! carrier-positions c)
            (hash-remove! attachments       c)))

        (when (Xfer? cmd)
          (check-target machine cmd)
          (let ([needle (OpN-needle cmd)]
                [target (OpNT-target cmd)])
            ;; move loop count
            (set-loops! machine target (+ (get-loops machine target)
                                          (get-loops machine needle)))
            (set-loops! machine needle 0))
          (move-attachments machine cmd))

        (when (Rack? cmd)
          (let ([old (MachineState-racking machine)]
                [new (Rack-racking cmd)])
            (when (= old new)
              (error 'fnitout "validating ~a:\nredundant Rack instruction" cmd))
            (unless (= 1 (abs (- old new)))
              (error 'fnitout "validating ~a:\nRack instruction can only change the racking by +/-1" cmd))
            (set-MachineState-racking! machine new)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns physical position of yarn carrier
(: carrier-physical-position (->* (MachineState Needle) (Dir) Integer))
(define (carrier-physical-position machine needle [dir '-])
  (let ([bed (Needle-bed needle)]
        [idx (Needle-index needle)])
    (+ idx
       (if (eq? '+ dir)
           1
           0)
       (if (eq? 'f bed)
           0
           (MachineState-racking machine)))))

;; check that all yarn carriers are at physical position corresponding to [n.x, dir]_r
(: check-carrier-positions : MachineState Command -> Void)
(define (check-carrier-positions machine cmd)
  (let* ([direction (command-opposite-dir cmd)] ;; NB command `Out` is a special case
         [needle    (command-needle cmd)]
         [carriers  (command-carriers cmd)]
         [expected  (carrier-physical-position machine needle direction)]
         [positions (MachineState-carrier-positions machine)])
    (for ([c (in-list carriers)])
      (let ([y (Carrier-val c)])
        (if (not (hash-has-key? positions y))
            (error 'fnitout "validating ~a:\nyarn carrier ~a is not in action" cmd y)
            (let ([actual (hash-ref positions y)])
              (unless (= expected actual)
                (error 'fnitout "validating ~a:\nexpected yarn carrier ~a at position ~a, but it is at ~a" cmd y expected actual))))))))

;; check that source and target needles are aligned
(: check-target : MachineState Command -> Void)
(define (check-target machine cmd)
  (let* ([needle (command-needle cmd)]
         [target (command-target cmd)])
    (when (eq? (Needle-bed needle)
               (Needle-bed target))
      (error 'fnitout "validating ~a:\nneedle and target are on same bed" cmd))
    (unless (= (carrier-physical-position machine needle)
               (carrier-physical-position machine target))
      (error 'fnitout "validating ~a:\nneedle and target are not aligned" cmd))))

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
;; NB. JL also sets direction of attachment
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
         [yarns (map Carrier-val carriers)])
    (for ([y (in-list yarns)])
      (hash-set! carrier-positions y (carrier-physical-position machine needle dir)))))

;; end