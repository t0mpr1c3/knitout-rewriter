#lang typed/racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide k2f)

(require threading)
(require "fnitout-command.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; script parameters

(define current-version
  : (Parameterof Natural)
  (make-parameter 0))
(define current-carriers
  : (Parameterof (Listof String))
  (make-parameter null))
(define current-gauge
  : (Parameterof Positive-Float)
  (make-parameter +inf.0))
#|
(define current-machine
  : (Parameterof String)
  (make-parameter ""))
(define current-width
  : (Parameterof Natural)
  (make-parameter 0))
(define current-position
  : (Parameterof String)
  (make-parameter ""))
(define current-yarns
  : (Parameterof (Listof (Pairof String String)))
  (make-parameter null))
(define current-extensions
  : (Parameterof (Listof String))
  (make-parameter null))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct CarrierLocation
  ([dir : Dir]
   [bed : Bed]
   [idx : Integer])
  #:transparent)

;; YarnCarrier struct
(struct YarnCarrier
  ([yarn         : Positive-Integer]
   [pending-idx  : (Option Natural)]
   [parked-loc   : (Option CarrierLocation)]
   [attached-loc : (Option CarrierLocation)])
  #:transparent)

(: carrier-pending? : YarnCarrier -> Boolean)
(define (carrier-pending? carrier)
  (not (false? (YarnCarrier-pending-idx carrier))))

(: carrier-parked? : YarnCarrier -> Boolean)
(define (carrier-parked? carrier)
  (not (false? (YarnCarrier-parked-loc carrier))))
#|
(: carrier-attached? : YarnCarrier -> Boolean)
(define (carrier-attached? carrier)
  (not (false? (YarnCarrier-attached-loc carrier))))
|#
;; constructor
(: make-YarnCarrier : Positive-Integer -> YarnCarrier)
(define (make-YarnCarrier y)
  (YarnCarrier y #f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse knitout from file
;; returns list of formal knitout commands
(: k2f : Path-String -> Script)
(define (k2f filename)
  (let* ([in (open-input-file filename)]
         [script (port->string in)])
    (knitout-parse script)))

(: knitout-parse : String -> Script)
(define (knitout-parse script)
  (~> script
      (string-split "\n")
      get-version
      get-headers
      get-pattern))

(: get-version : (Listof String) -> (Listof String))
(define (get-version lines)
  (if (null? lines)
      (error 'fnitout "invalid knitout magic string")
      (let* ([line (car lines)]
             [m (regexp-match #px"^;!knitout-(\\d+)$" line)])
        (if (false? m)
            (error 'fnitout "invalid knitout magic string")
            (let ([v (cadr m)])
              (assert (string? v))
              (let ([version (string->number v)])
                (assert (natural? version))
                (when (> version 2)
                  (displayln (format "Warning: Knitout is version ~a, but this code only knows about versions up to 2." version)))
                (current-version version)
                (cdr lines)))))))

(: get-headers : (Listof String) -> (Listof String))
(define (get-headers header)
  (let ([pattern : (Listof String)
          (let loop ([lines header])
            (if (null? lines)
                null
                (let* ([line (car lines)]
                       [m (regexp-match #px"^;;(.*?): (.*)" line)])
                  (if (false? m)
                      lines
                      (let ([param (second m)]
                            [value (third  m)])
                        (assert (string? param))
                        (assert (string? value))
                        (parse-header param value)                    
                        (loop (cdr lines)))))))]) ;; next line
    (when (null? (current-carriers))
      (error 'fnitout "Carriers header not included but is required"))
    (if (infinite? (current-gauge))
        (begin
          (current-gauge 15.0) ;; set default
          (displayln (format "Gauge header not specified. Assuming needles are 1 / ~a inches apart." (current-gauge))))
        (displayln (format "Gauge header indicates needles are 1 / ~a inches apart." (current-gauge))))
    pattern))

(: parse-header : String String -> Void)
(define (parse-header param value)
  (let ([positive-float? (make-predicate Positive-Float)])         
    (cond [(equal? "Carriers" param)
           (current-carriers (string-split value))]
          [(equal? "Gauge" param)
           (let ([gauge (string->number value)])
             (if (false? gauge)
                 (error 'fnitout "gauge value (~a) should be a number greater than zero" value)
                 (let ([gauge~ (exact->inexact gauge)])
                   (if (not (positive-float? gauge~))
                       (error 'fnitout "gauge value (~a) should be a number greater than zero" value)
                       (current-gauge gauge~)))))]
          [(or (string-prefix? param "Yarn-")
               (string-prefix? param "X-")
               (equal? param "Machine")
               (equal? param "Width")
               (equal? param "Position"))
           (void)] ;; ignore
          [else
           (displayln (format "Warning: File contains unknown comment header '~a'" param))])))

(: get-pattern : (Listof String) -> Script)
(define (get-pattern pattern)
  (let ([carriers (make-integer-carriers (current-carriers))]
        [loops : (HashTable Needle Natural) (make-hash)]) ;; not hasheq
    (let next-line ([lines    pattern]
                    [racking  : Integer 0]
                    [output   : (Listof Instruction) null])
      (if (null? lines)
          (reverse output)

          ;; parse line
          (let* ([original (car lines)]
                 [m (regexp-match #px"^(.*?)(;.*)" original)]
                 [line (if (false? m)
                           original
                           (second m))]
                 [comment (if (false? m)
                              ""
                              (third m))])
            (assert (string? line))
            (assert (string? comment))
            (let ([tokens (string-split (string-trim line))])
              (if (zero? (length tokens))
                  ;; skip empty line
                  (next-line (cdr lines)
                             racking
                             (if (zero? (string-length comment))
                                 output
                                 (cons (Instruction (Nop) comment)
                                       output)))
                  ;; handle operations
                  (let ([op   (car tokens)]
                        [args (cdr tokens)])

                    (cond
                      [(or (equal? op "releasehook")    ;; ignore
                           (equal? op "x-presser-mode")
                           (equal? op "x-speed-number")
                           (equal? op "pause"))
                       (next-line (cdr lines)
                                  racking
                                  output)]

                      [(or (equal? op "in")
                           (equal? op "inhook"))
                       (if (zero? (length args))
                           (error 'fnitout "can't bring in no carriers")
                           (let in-loop ([cs        args]
                                         [original~ original]
                                         [output~   output])
                             (if (null? cs)
                                 (next-line (cdr lines)
                                            racking
                                            output~)
                                 (let ([cn (car cs)])
                                   (unless (hash-has-key? carriers cn)
                                     (error 'fnitout "carrier ~a not named in Carriers comment header" cn))
                                   (let ([carrier (hash-ref carriers cn)])
                                     (when (carrier-parked? carrier)
                                       (error 'fnitout "yarn carrier ~a is already in" cn))
                                     (when (carrier-pending? carrier)
                                       (error 'fnitout "yarn carrier ~a is pending" cn))
                                     (hash-set! carriers cn
                                                (struct-copy YarnCarrier carrier
                                                             [pending-idx (length output~)]))
                                     (in-loop (cdr cs)
                                              ""
                                              (cons (Instruction (Nop) original~) ;; placeholder for In command
                                                    output~)))))))]

                      [(or (equal? op "out")
                           (equal? op "outhook"))
                       (if (zero? (length args))
                           (error 'fnitout "can't take out no carriers")
                           (let out-loop ([cs        args]
                                          [original~ original]
                                          [output~   output])
                             (if (null? cs)
                                 (next-line (cdr lines)
                                            racking
                                            output~)
                                 (let ([cn (car cs)])
                                   (unless (hash-has-key? carriers cn)
                                     (error 'fnitout "carrier ~a not named in Carriers comment header" cn))
                                   (let ([carrier (hash-ref carriers cn)])
                                     (if (carrier-pending? carrier)
                                         (error 'fnitout "yarn carrier ~a is pending" cn)
                                         (let ([parked-loc (YarnCarrier-parked-loc carrier)])
                                           (if (false? parked-loc)
                                               (error 'fnitout "yarn carrier ~a is not in" cn)
                                               (let* ([parked-dir (CarrierLocation-dir parked-loc)]
                                                      [parked-bed (CarrierLocation-bed parked-loc)]
                                                      [parked-idx (CarrierLocation-idx parked-loc)]
                                                      [out-needle (Needle
                                                                   parked-bed
                                                                   (+ parked-idx
                                                                      (if (eq? '+ parked-dir) 1 -1)))]                                                                        
                                                      [cmd (Out (Direction parked-dir)
                                                                out-needle
                                                                (Carrier (YarnCarrier-yarn carrier)))])                                                             
                                                 (hash-set! carriers cn
                                                            (struct-copy YarnCarrier carrier
                                                                         [parked-loc   #f]
                                                                         [attached-loc #f]))
                                                 (out-loop (cdr cs)
                                                           ""
                                                           (cons (Instruction cmd original~)
                                                                 output~)))))))))))]

                      ;; TODO (JL): miss back-bed-attached carriers over
                      [(equal? op "rack")
                       (if (not (= 1 (length args)))
                           (error 'fnitout "rack instruction takes one argument")
                           (let ([racking~ (string->number (car args))])
                             (if (or (false? racking~)
                                     (not (real? racking~))
                                     (infinite? racking~))
                                 (error 'fnitout "racking must be a number")
                                 (if (not (exact-integer? racking~))
                                     (error 'fnitout "fractional pitch racking not yet supported")
                                     (if (= racking racking~)
                                         (next-line (cdr lines)
                                                    racking~
                                                    (cons (Instruction (Nop) original) ;; redundant Rack command
                                                          output))
                                         (let ([s (sign (- racking~ racking))])
                                           (let next-rack ([r         racking]
                                                           [original~ original]
                                                           [output~   output])
                                             (if (= r racking~)
                                                 (next-line (cdr lines)
                                                            racking~
                                                            output~)
                                                 (next-rack (+ r s)
                                                            ""
                                                            (cons (Instruction (Rack (+ r s)) original~)
                                                                  output~))))))))))]

                      ;; Stitch instruction is ignored
                      ;; TODO (JL): maybe remember these values for loop lengths?
                      [(equal? op "stitch")
                       (if (not (= 2 (length args)))
                           (error 'fnitout "stitch instruction takes two arguments")
                           (let ([s1 (string->number (car args))]
                                 [s2 (string->number (cadr args))])
                             (if (or (false? s1)
                                     (false? s2)
                                     (not (real? s1))
                                     (not (real? s2))
                                     (not (exact-integer? s1))
                                     (not (exact-integer? s2)))
                                 (error 'fnitout "stitch arguments must be integers")
                                 (next-line (cdr lines)
                                            racking
                                            output))))]

                      ;; X-stitch-number instruction is ignored
                      ;; TODO (JL): maybe remember this value for loop lengths?
                      [(equal? op "x-stitch-number")
                       (if (not (= 1 (length args)))
                           (error 'fnitout "x-stitch-number instruction takes one argument")
                           (let ([n (string->number (car args))])
                             (if (or (false? n)
                                     (not (real? n))
                                     (not (exact-integer? n))
                                     (negative? n))
                                 (error 'fnitout "x-stitch-number argument must be non-negative integer")
                                 (next-line (cdr lines)
                                            racking
                                            output))))]

                      [(or (equal? op "miss")
                           (equal? op "tuck")
                           (equal? op "knit")
                           (equal? op "split")
                           (equal? op "amiss")
                           (equal? op "drop")
                           (equal? op "xfer"))
                       ;; Amiss is synonym for Tuck  in direction + with no carriers
                       ;; Drop  is synonym for Knit  in direction + with no carriers
                       ;; Xfer  is synonym for Split in direction + with no carriers
                       (let* ([synonym? (or (equal? op "amiss")
                                            (equal? op "drop")
                                            (equal? op "xfer"))]
                              [args~ (if synonym?
                                         (cons "+" args)
                                         args)]
                              [d (parse-dir (first args~))]
                              [n (parse-needle (second args~))]
                              [b (Needle-bed n)]
                              [i (Needle-index n)]
                              [target? (or (equal? op "split")
                                           (equal? op "xfer"))]
                              [t : (Option Needle)
                                 (if target?
                                     (parse-needle (third args~))
                                     #f)]
                              [cs (drop args~ (if target? 3 2))]
                              [yarn-count (length cs)])
                         (when (and synonym?
                                    (not (zero? yarn-count)))
                           (error 'fnitout "cannot amiss/drop/xfer with carriers (use tuck/knit/split)"))
                         (when (and (zero? yarn-count)
                                    (equal? op "miss"))
                           (error 'fnitout "it makes no sense to miss with no yarns"))
                             
                         ;; set up carriers
                         (let* ([before-idx (+ i
                                               (if (eq? d '+) -1 +1))]
                                [before-idxb (+ before-idx
                                                (if (eq? 'b b) racking 0))])
                           (let-values
                               ([(output1 original1)
                                 (let next-carrier : (values (Listof Instruction) String)
                                   ([c-cs       cs]
                                    [c-original original]
                                    [c-output   output])
                                   (if (null? c-cs)
                                       (values c-output c-original)
                                       (let ([cn (car c-cs)])
                                         (unless (hash-has-key? carriers cn)
                                           (error 'fnitout "carrier ~a not named in Carriers comment header" cn))
                                         (let* ([carrier (hash-ref carriers cn)]                                                   
                                                [c (Carrier (YarnCarrier-yarn carrier))]
                                                [idx (YarnCarrier-pending-idx carrier)])
                                           (if (not (false? idx))
                                               ;; carrier is pending
                                               ;; retroactively bring carrier in:
                                               ;; use front bed location to avoid back-bed location having different meaning in patched instruction
                                               (let* ([idx~ (- (length c-output) idx 1)] ;; output is accumulated backwards
                                                      [before-loc (CarrierLocation d b before-idx)]
                                                      [cmd (In (Direction d)
                                                               (Needle 'f before-idxb)
                                                               c)]
                                                      [comment (Instruction-comment (list-ref c-output idx~))])
                                                 (hash-set! carriers cn
                                                            (struct-copy YarnCarrier   carrier
                                                                         [pending-idx  #f]
                                                                         [parked-loc   before-loc]
                                                                         [attached-loc before-loc]))
                                                 (next-carrier (cdr c-cs)
                                                               c-original
                                                               (append
                                                                (take c-output idx~)
                                                                (list (Instruction cmd comment))
                                                                (drop c-output (add1 idx~))
                                                                )))
                                               (if (carrier-parked? carrier)
                                                   ;; carrier is parked
                                                   ;; move carrier to just before needle
                                                   (let ([target (+ before-idxb
                                                                    (if (eq? '+ d) +0.5 -0.5))])
                                                     (let next-miss ([m-original c-original]
                                                                     [m-output   c-output])
                                                       ;; update carrier information
                                                       (let* ([carrier (hash-ref carriers cn)]
                                                              [parked-loc (YarnCarrier-parked-loc carrier)])
                                                         (assert (not (false? parked-loc)))
                                                         (let* ([parked-dir (CarrierLocation-dir parked-loc)]
                                                                [parked-bed (CarrierLocation-bed parked-loc)]
                                                                [parked-idx (CarrierLocation-idx parked-loc)]
                                                                [pos (+ parked-idx
                                                                        (if (eq? '+ parked-dir) +0.5 -0.5)
                                                                        (if (eq? 'b parked-bed) racking 0))])
                                                           (if (= pos target)
                                                               (next-carrier (cdr c-cs)
                                                                             m-original
                                                                             m-output)
                                                               (if (< pos target)
                                                                   ;; pos < target
                                                                   (let* ([parked-dir~ '+]
                                                                          [parked-idx~
                                                                           (if (eq? '- parked-dir)
                                                                               parked-idx
                                                                               (add1 parked-idx))]
                                                                          [parked-loc~ (CarrierLocation parked-dir~
                                                                                                        parked-bed
                                                                                                        parked-idx~)]
                                                                          [carrier~ (struct-copy YarnCarrier carrier
                                                                                                 [parked-loc parked-loc~])]
                                                                          [cmd (Miss (Direction '+)
                                                                                     (Needle parked-bed
                                                                                             parked-idx~)
                                                                                     c)])
                                                                     (hash-set! carriers cn carrier~)
                                                                     (next-miss ""
                                                                                (cons (Instruction cmd m-original)
                                                                                      m-output)))
                                                                   ;; pos > target
                                                                   (let* ([parked-dir~ '-]
                                                                          [parked-idx~
                                                                           (if (eq? '+ parked-dir)
                                                                               parked-idx
                                                                               (sub1 parked-idx))]
                                                                          [parked-loc~ (CarrierLocation parked-dir~
                                                                                                        parked-bed
                                                                                                        parked-idx~)]
                                                                          [carrier~ (struct-copy YarnCarrier carrier
                                                                                                 [parked-loc parked-loc~])]
                                                                          [cmd (Miss (Direction '-)
                                                                                     (Needle parked-bed
                                                                                             parked-idx~)
                                                                                     c)])
                                                                     (hash-set! carriers cn carrier~)
                                                                     (next-miss ""
                                                                                (cons (Instruction cmd m-original)
                                                                                      m-output)))))))))
                                                   (error 'fnitout "carrier ~a is not pending or in" cn)))))))])

                             ;; operation name decay based on loop presence
                             (let* ([op~ (if (and (or (equal? op "knit")
                                                      (equal? op "split")
                                                      (equal? op "drop")  ;; synonym
                                                      (equal? op "xfer")) ;; synonym
                                                  (not (hash-has-key? loops n)))
                                             "tuck"
                                             op)]

                                    ;; calculate yarn lengths
                                    [stitchsize : Positive-Flonum 30.0] ;; TODO (JL): actually figure out how to set this (maybe using 'stitch'?)
                                    [w 0.0] ;; "needle width"
                                    [pos (+ i
                                            (if (eq? 'b b) racking 0)
                                            (if (eq? '+ d) (- w) w))]
                                    [yarns (for/list ([cn (in-list cs)]) : (Listof Yarn)
                                             (let* ([carrier (hash-ref carriers cn)]
                                                    [c (YarnCarrier-yarn carrier)]
                                                    [attached-loc (YarnCarrier-attached-loc carrier)])
                                               (assert (not (false? attached-loc)))
                                               (let* ([attached-dir (CarrierLocation-dir attached-loc)]
                                                      [attached-bed (CarrierLocation-bed attached-loc)]
                                                      [attached-idx (CarrierLocation-idx attached-loc)]
                                                      [attached-pos (+ attached-idx
                                                                       (if (eq? 'b attached-bed) racking 0)
                                                                       (if (eq? '+ attached-dir) (- w) w))]
                                                      [len (+ (abs (- pos attached-pos))
                                                              1e-16)]) ;; to ensure a positive value
                                                 (Yarn (Carrier c)
                                                       (Length len)))))]

                                    ;; output instruction
                                    [output2
                                     (cond
                                       [(equal? op~ "miss")
                                        (let x-loop : (Listof Instruction)
                                          ([x-cs cs]
                                           [x-original original1]
                                           [x-output output1])
                                          (if (null? x-cs)
                                              x-output
                                              (let* ([cn (car x-cs)]
                                                     [carrier (hash-ref carriers cn)]                                                   
                                                     [c (YarnCarrier-yarn carrier)]
                                                     [cmd (Miss (Direction d)
                                                                n
                                                                (Carrier c))])
                                                (x-loop (cdr x-cs)
                                                        ""
                                                        (cons (Instruction cmd x-original)
                                                              x-output)))))]
                                       [(or (equal? op~ "amiss")
                                            (equal? op~ "tuck"))
                                        (if (zero? yarn-count)
                                            (cons (Instruction (Nop)
                                                               (string-append original1 " (amiss ignored)"))
                                                  output1)
                                            (let ([cmd (Tuck
                                                        (Direction d)
                                                        n
                                                        (Length stitchsize)
                                                        (car yarns))]
                                                  [n-loops (hash-ref loops n (thunk 0))])
                                              (hash-set! loops n (+ n-loops
                                                                    yarn-count))
                                              (cons (Instruction cmd original1)
                                                    output1)))]
                                       [(or (equal? op~ "drop")
                                            (equal? op~ "knit"))
                                        (if (zero? yarn-count)
                                            (begin
                                              (hash-remove! loops n)
                                              (cons (Instruction (Drop n) original1)
                                                    output1))
                                            (let ([cmd (Knit
                                                        (Direction d)
                                                        n
                                                        (Length stitchsize)
                                                        yarns)])
                                              (hash-set! loops n yarn-count)
                                              (cons (Instruction cmd original1)
                                                    output1)))]
                                       [(or (equal? op~ "xfer")
                                            (equal? op~ "split"))
                                        (begin
                                          (assert (Needle? t))
                                          (let ([cmd
                                                 (if (zero? yarn-count)
                                                     (Xfer n t)
                                                     (Split (Direction d)
                                                            n
                                                            t
                                                            (Length stitchsize)
                                                            yarns))]
                                                [n-loops (hash-ref loops n (thunk 0))]
                                                [t-loops (hash-ref loops t (thunk 0))])
                                            (if (zero? yarn-count)
                                                (hash-remove! loops n)
                                                (hash-set! loops n yarn-count))
                                            (hash-set! loops t (+ t-loops n-loops))
                                            #|
                                            ;; unnecessary: operation should have decayed
                                            (when (and (zero? t-loops)
                                                       (zero? n-loops))
                                              (hash-remove! loops t))
                                            |#
                                            ;; update carrier attachments & parkings
                                            ;; (for *all* carriers -- though carriers in cs will get this info overwritten)
                                            (for ([cn (in-hash-keys carriers)])
                                              (let* ([carrier (hash-ref carriers cn)]
                                                     [parked-loc   (YarnCarrier-parked-loc carrier)]
                                                     [attached-loc (YarnCarrier-attached-loc carrier)])
                                                (when (and (not (false? parked-loc))
                                                           (not (false? attached-loc)))
                                                  (let* ([parked-dir (CarrierLocation-dir parked-loc)]
                                                         [parked-bed (CarrierLocation-bed parked-loc)]
                                                         [parked-idx (CarrierLocation-idx parked-loc)]
                                                         [parked-loc~
                                                          (if (and (eq? parked-bed b)
                                                                   (eq? parked-idx i))
                                                              (CarrierLocation parked-dir
                                                                               (Needle-bed t)
                                                                               (Needle-index t))
                                                              parked-loc)]
                                                         [attached-dir (CarrierLocation-dir attached-loc)]
                                                         [attached-bed (CarrierLocation-bed attached-loc)]
                                                         [attached-idx (CarrierLocation-idx attached-loc)]
                                                         [attached-loc~
                                                          (if (and (eq? attached-bed b)
                                                                   (eq? attached-idx i))
                                                              (CarrierLocation attached-dir
                                                                               (Needle-bed t)
                                                                               (Needle-index t))
                                                              attached-loc)]
                                                         [carrier~ (struct-copy YarnCarrier carrier
                                                                                [parked-loc   parked-loc~]
                                                                                [attached-loc attached-loc~])])
                                                    (hash-set! carriers cn carrier~)))))
                                            (cons (Instruction cmd original1)
                                                  output1)))]
                                       [else (error 'fnitout "operation ~a should not be processed here" op)])])
                               ;; update carrier attachments + parkings
                               (for ([cn (in-list cs)])
                                 (let* ([carrier (hash-ref carriers cn)]
                                        [attached-loc (YarnCarrier-attached-loc carrier)]
                                        [loc~ (CarrierLocation d b i)]
                                        [parked-loc~   loc~]
                                        [attached-loc~ (if (equal? op "miss")
                                                           attached-loc
                                                           loc~)]
                                        [carrier~ (struct-copy YarnCarrier carrier
                                                               [parked-loc   parked-loc~]
                                                               [attached-loc attached-loc~])])
                                   (hash-set! carriers cn carrier~)))
                               (next-line (cdr lines)
                                          racking
                                          output2)))))]
                      [(string-prefix? op "x-")
                       (begin
                         (displayln (format "Warning: unsupported extension operation '~a'." op))
                         (next-line (cdr lines)
                                    racking
                                    output))]
                      [else
                       (error 'fnitout "unsupported operation '~a'." op)])))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-integer-carriers : (Listof String) -> (HashTable String YarnCarrier))
(define (make-integer-carriers carriers)
  (if (let loop : Boolean ([cns carriers]
                           [prev 0])
        (if (null? cns)
            #f
            (let ([c (string->number (car cns))])
              (if (or (false? c)
                      (not (real? c))
                      (not (exact-integer? c))
                      (not (> c prev)))
                  #t
                  (loop (cdr cns)
                        c)))))
      (let ([remap             
             (for/list ([i : Natural (in-range (length carriers))]
                        [j (in-list carriers)]) : (Listof (Pairof String YarnCarrier))
               (cons j (make-YarnCarrier (add1 i))))])
        (displayln "Carrier names were not integers in order, so using position-based remapping.")
        (make-hash remap))
      (let ([remap
             (for/list ([j (in-list carriers)]) : (Listof (Pairof String YarnCarrier))
               (let ([i (string->number j)])
                 (assert (exact-positive-integer? i))
                 (cons j (make-YarnCarrier i))))])
        (displayln (format "Will use carrier names ~a directly as yarn numbers." (string-join carriers)))
        (make-hash remap))))

(: sign : Real -> Integer)
(define (sign x)
  (if (positive? x)
      +1
      (if (negative? x)
          -1
          0)))

(: parse-dir : String -> Dir)
(define (parse-dir arg)
  (if (equal? "+" arg)
      '+
      (if (equal? "-" arg)
          '-
          (error "invalid direction specification ~a" arg))))

(: parse-needle : String -> Needle)
(define (parse-needle arg)
  (let ([m (regexp-match #px"^([fb]s?)(-?\\d+)$" arg)])
    (if (false? m)
        (error 'fnitout "invalid needle specification ~a" arg)
        (let ([b (second m)]
              [i (third  m)])
          (assert (string? b))
          (assert (string? i))
          (let ([bed (string->symbol b)]
                [idx (string->number i)])
            (when (not (Bed? bed))
              (error 'fnitout "sliders not yet supported by translation code"))
            (assert (exact-integer? idx))
            (Needle bed idx))))))

#|
;; test
(define f (k2f "../../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.k"))
(display (script->string f))
(with-output-to-file "one-fourth.f"
  (thunk (script-export f)))
|#

;; end
