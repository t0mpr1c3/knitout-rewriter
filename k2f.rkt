#lang typed/racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide k2f)

(require threading)
(require "fnitout-command.rkt"
         "fnitout-serialization.rkt")

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

(define-type Carriers
  (Immutable-HashTable String YarnCarrier))

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

(define-type
  Loops (Immutable-HashTable Needle Natural))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse knitout from file
;; returns list of formal knitout commands
(: k2f : Path-String -> Script)
(define (k2f filename)
  (let* ([in (open-input-file filename)]
         [script (port->string in)])
    (knitout-parse script)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: knitout-parse : String -> Script)
(define (knitout-parse script)
  (~> script
      (string-split "\n")
      get-version
      get-headers
      get-pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; now featuring immutable hashtables!
(: get-pattern : (Listof String) -> Script)
(define (get-pattern pattern)
  (next-line pattern
             0
             (make-integer-carriers (current-carriers))
             (hash)
             null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-integer-carriers : (Listof String) -> Carriers)
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
        (make-immutable-hash remap))
      (let ([remap
             (for/list ([j (in-list carriers)]) : (Listof (Pairof String YarnCarrier))
               (let ([i (string->number j)])
                 (assert (exact-positive-integer? i))
                 (cons j (make-YarnCarrier i))))])
        (displayln (format "Will use carrier names ~a directly as yarn numbers." (string-join carriers)))
        (make-immutable-hash remap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: next-line : (Listof String) Integer Carriers Loops Script -> Script)
(define (next-line lines racking carriers loops output)
  (if (null? lines)
      (reverse output)

      ;; parse line
      (let ([original (car lines)])
        (let-values ([(tokens comment)
                      (parse-line original)])

          ;; skip empty line
          (if (zero? (length tokens))
              (let ([output~
                     (if (zero? (string-length comment))
                         output
                         (cons (Instruction (Nop) comment)
                               output))])
              (next-line (cdr lines)
                         racking
                         carriers
                         loops
                         output~))

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
                              carriers
                              loops
                              output)]

                  [(or (equal? op "in")
                       (equal? op "inhook"))
                   (let-values ([(carriers~ output~)
                                 (inhook original args carriers output)])
                     (next-line (cdr lines)
                                racking
                                carriers~
                                loops
                                output~))]

                  [(or (equal? op "out")
                       (equal? op "outhook"))
                   (let-values ([(carriers~ output~)
                                 (outhook original args carriers output)])
                     (next-line (cdr lines)
                                racking
                                carriers~
                                loops
                                output~))]

                  ;; TODO (JL): miss back-bed-attached carriers over
                  [(equal? op "rack")
                   (let-values ([(racking~ output~)
                                 (rack original args racking output)])
                     (next-line (cdr lines)
                                racking~
                                carriers
                                loops
                                output~))]

                  ;; Stitch instruction is ignored
                  ;; TODO (JL): maybe remember these values for loop lengths?
                  [(equal? op "stitch")
                   (begin
                     (stitch args)
                     (next-line (cdr lines)
                                racking
                                carriers
                                loops
                                output))]

                  ;; X-stitch-number instruction is ignored
                  ;; TODO (JL): maybe remember this value for loop lengths?
                  [(equal? op "x-stitch-number")
                   (begin
                     (x-stitch-number args)
                     (next-line (cdr lines)
                                racking
                                carriers
                                loops
                                output))]

                  [(or (equal? op "miss")
                       (equal? op "tuck")
                       (equal? op "knit")
                       (equal? op "split")
                       (equal? op "amiss")
                       (equal? op "drop")
                       (equal? op "xfer"))
                   (let-values ([(carriers~ loops~ output~)
                                 (tuck-knit-split original op args racking carriers loops output)])
                     (next-line (cdr lines)
                                racking
                                carriers~
                                loops~
                                output~))]

                  [(string-prefix? op "x-")
                   (begin
                     (displayln (format "Warning: unsupported extension operation '~a'." op))
                     (next-line (cdr lines)
                                racking
                                carriers
                                loops
                                output))]
                  [else
                   (error 'fnitout "unsupported operation '~a'." op)])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: parse-line : String -> (values (Listof String) String))
(define (parse-line original)
  (let* ([m (regexp-match #px"^(.*?)(;.*)" original)]
         [line (if (false? m)
                   original
                   (second m))]
         [comment (if (false? m)
                      ""
                      (third m))])
    (assert (string? line))
    (assert (string? comment))
    (let ([tokens (string-split (string-trim line))])
      (values tokens comment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: inhook : String (Listof String) Carriers Script -> (values Carriers Script))
(define (inhook original args carriers output)
  (if (zero? (length args))
      (error 'fnitout "can't bring in no carriers")
      (inhook-aux args original carriers output)))

(: inhook-aux : (Listof String) String Carriers Script -> (values Carriers Script))
(define (inhook-aux cs original carriers output)
  (if (null? cs)
      (values carriers output)
      (let ([cn (car cs)])
        (when (not (hash-has-key? carriers cn))
          (error 'fnitout "carrier ~a not named in Carriers comment header" cn))
        (let ([carrier (hash-ref carriers cn)])
          (when (carrier-parked? carrier)
            (error 'fnitout "yarn carrier ~a is already in" cn))
          (when (carrier-pending? carrier)
            (error 'fnitout "yarn carrier ~a is pending" cn))                                     
          (inhook-aux (cdr cs)
                      ""
                      (hash-set carriers cn
                                (struct-copy YarnCarrier carrier
                                             [pending-idx (length output)]))
                      (cons (Instruction (Nop) original) ;; placeholder for In command
                            output))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: outhook : String (Listof String) Carriers Script -> (values Carriers Script))
(define (outhook original args carriers output)
  (if (zero? (length args))
      (error 'fnitout "can't take out no carriers")
      (outhook-aux args original carriers output)))
      
(: outhook-aux : (Listof String) String Carriers Script -> (values Carriers Script))
(define (outhook-aux cs original carriers output)
  (if (null? cs)
      (values carriers output)
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
                           [out-needle (Needle parked-bed
                                               (+ parked-idx
                                                  (if (eq? '+ parked-dir) 1 -1)))]                                                                        
                           [cmd (Out (Direction parked-dir)
                                     out-needle
                                     (Carrier (YarnCarrier-yarn carrier)))]
                           [carriers~
                            (hash-set carriers cn
                                      (struct-copy YarnCarrier carrier
                                                   [parked-loc   #f]
                                                   [attached-loc #f]))]
                           [output~
                            (cons (Instruction cmd original)
                                  output)])
                      (outhook-aux (cdr cs)                                                          
                                   ""
                                   carriers~
                                   output~)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: rack : String (Listof String) Integer Script -> (values Integer Script))
(define (rack original args racking output)
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
                    (values racking~
                            (cons (Instruction (Nop)
                                               (string-append original " (ignored)")) ;; redundant Rack command
                                  output))
                    (let ([s (sign (- racking~ racking))])
                      (let next-rack ([r         racking]
                                      [original~ original]
                                      [output~   output])
                        (if (= r racking~)
                            (values racking~ output~)
                            (next-rack (+ r s)
                                       ""
                                       (cons (Instruction (Rack (+ r s)) original~)
                                             output~)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: sign : Real -> Integer)
(define (sign x)
  (if (positive? x)
      +1
      (if (negative? x)
          -1
          0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: stitch : (Listof String) -> Void)
(define (stitch args)
  (if (not (= 2 (length args)))
      (error 'fnitout "stitch instruction takes two arguments")
      (let ([s1 (string->number (car args))]
            [s2 (string->number (cadr args))])
        (when (or (false? s1)
                  (false? s2)
                  (not (real? s1))
                  (not (real? s2))
                  (not (exact-integer? s1))
                  (not (exact-integer? s2)))
          (error 'fnitout "stitch arguments must be integers")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: x-stitch-number : (Listof String) -> Void)
(define (x-stitch-number args)
  (if (not (= 1 (length args)))
      (error 'fnitout "x-stitch-number instruction takes one argument")
      (let ([n (string->number (car args))])
        (when (or (false? n)
                  (not (real? n))
                  (not (exact-integer? n))
                  (negative? n))
          (error 'fnitout "x-stitch-number argument must be non-negative integer")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: tuck-knit-split :
   String String (Listof String) Integer Carriers Loops Script ->
   (values Carriers Loops Script))
(define (tuck-knit-split original op args racking carriers loops output)  
  ;; Amiss is synonym for Tuck  in direction + with no carriers
  ;; Drop  is synonym for Knit  in direction + with no carriers
  ;; Xfer  is synonym for Split in direction + with no carriers
  (let* ([synonym? (or (equal? op "amiss")
                       (equal? op "drop")
                       (equal? op "xfer"))]
         [args1 (if synonym?
                    (cons "+" args)
                    args)]
         [d (parse-dir (first args1))]
         [n (parse-needle (second args1))]
         [target? (or (equal? op "split")
                      (equal? op "xfer"))]
         [t : (Option Needle)
            (if target?
                (parse-needle (third args1))
                #f)]
         [cs (drop args1 (if target? 3 2))]
         [yarn-count (length cs)])
    (when (and synonym?
               (not (zero? yarn-count)))
      (error 'fnitout "cannot amiss/drop/xfer with carriers (use tuck/knit/split)"))
    (when (and (zero? yarn-count)
               (equal? op "miss"))
      (error 'fnitout "it makes no sense to miss with no yarns"))
    (let*-values
        ([(carriers2 original2 output2)
          (setup-carriers d n racking cs carriers original output)]
         [(carriers3 loops3 output3)
          (do-operation op d n t cs
                        (calculate-yarn-lengths d n cs racking carriers2)
                        carriers2 loops original2 output2)])
      (values (update-attachments op d n cs carriers3)
              loops3
              output3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: parse-dir : String -> Dir)
(define (parse-dir arg)
  (if (equal? "+" arg)
      '+
      (if (equal? "-" arg)
          '-
          (error "invalid direction specification ~a" arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: setup-carriers :
   Dir Needle Integer (Listof String) Carriers String Script ->
   (values Carriers String Script))
(define (setup-carriers d n racking cs carriers original output)
  (let* ([b (Needle-bed n)]
         [i (Needle-index n)]
         [before-idx (+ i
                        (if (eq? d '+) -1 +1))]
         [before-idxb (+ before-idx
                         (if (eq? 'b b) racking 0))])
    (let setup-carriers-aux ([cs1       cs]
                             [carriers1 : Carriers carriers]
                             [original1 original]
                             [output1   output])
      (if (null? cs1)
          (values carriers1 original1 output1)
          (let ([cn (car cs1)])
            (unless (hash-has-key? carriers1 cn)
              (error 'fnitout "carrier ~a not named in Carriers comment header" cn))
            (let* ([carrier (hash-ref carriers1 cn)]
                   [idx (YarnCarrier-pending-idx carrier)])
              (if (not (false? idx))
                  ;; carrier is pending
                  ;; retroactively bring carrier in:
                  ;; use front bed location to avoid back-bed location
                  ;; having different meaning in patched instruction
                  (let* ([idx~ (- (length output) idx 1)] ;; output is accumulated backwards
                         [before-loc (CarrierLocation d b before-idx)]
                         [carriers2
                          (hash-set carriers1 cn
                                    (struct-copy YarnCarrier   carrier
                                                 [pending-idx  #f]
                                                 [parked-loc   before-loc]
                                                 [attached-loc before-loc]))]
                         [cmd (In (Direction d)
                                  (Needle 'f before-idxb)
                                  (Carrier (YarnCarrier-yarn carrier)))]
                         [comment (Instruction-comment (list-ref output1 idx~))]
                         [output2 (append
                                   (take output1 idx~)
                                   (list (Instruction cmd comment))
                                   (drop output1 (add1 idx~)))])
                    (setup-carriers-aux (cdr cs)
                                        carriers2
                                        original1
                                        output2))
                  (if (carrier-parked? carrier)
                      ;; carrier is parked
                      ;; move carrier to just before needle
                      (let ([target (+ before-idxb
                                       (if (eq? '+ d) +0.5 -0.5))])
                        (let-values ([(carriers3 original3 output3)
                                      (move-carrier target cn racking carriers original output)])
                          (setup-carriers-aux (cdr cs)
                                              carriers3
                                              original3
                                              output3)))
                      (error 'fnitout "carrier ~a is not pending or in" cn)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: move-carrier : Real String Integer Carriers String Script -> (values Carriers String Script))
(define (move-carrier target cn racking p-carriers p-original p-output)
  (let loop ([carriers : Carriers p-carriers]
             [original p-original]
             [output   p-output])
  (let* ([carrier (hash-ref carriers cn)]
         [c (Carrier (YarnCarrier-yarn carrier))]
         [parked-loc (YarnCarrier-parked-loc carrier)])
    (assert (not (false? parked-loc)))
    (let* ([parked-dir (CarrierLocation-dir parked-loc)]
           [parked-bed (CarrierLocation-bed parked-loc)]
           [parked-idx (CarrierLocation-idx parked-loc)]
           [pos (+ parked-idx
                   (if (eq? '+ parked-dir) +0.5 -0.5)
                   (if (eq? 'b parked-bed) racking 0))])
      (if (= pos target)
          (values carriers original output)
          (let-values ([(carriers~ cmd)                            
                        (move-carrier-aux (if (< pos target) '+ '-)
                                          parked-dir parked-bed parked-idx cn c carriers)])
            (let ([output~ (cons (Instruction cmd original)
                                 output)])
              (loop carriers~
                    ""
                    output~))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: move-carrier-aux : Dir Dir Bed Integer String Carrier Carriers -> (values Carriers Command))
(define (move-carrier-aux dir~ dir bed idx cn c carriers)
  (let* ([idx~ (if (eq? dir dir~)
                   (+ idx (if (eq? '+ dir~) +1 -1))
                   idx)]
         [loc~ (CarrierLocation dir~ bed idx~)]
         [carrier (hash-ref carriers cn)]
         [carrier~ (struct-copy YarnCarrier carrier
                                [parked-loc loc~])]
         [carriers~ (hash-set carriers cn carrier~)]
         [cmd (Miss (Direction dir~)
                    (Needle bed idx~)
                    c)])
    (values carriers~ cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(: calculate-yarn-lengths : Dir Needle (Listof String) Integer Carriers -> (Listof Yarn))
(define (calculate-yarn-lengths d n cs racking carriers2)
  (let* ([w 0.0] ;; "needle width"         
         [b (Needle-bed n)]
         [i (Needle-index n)]
         [pos (+ i
                 (if (eq? 'b b) racking 0)
                 (if (eq? '+ d) (- w) w))])
    (for/list ([cn (in-list cs)]) : (Listof Yarn)
      (let* ([carrier (hash-ref carriers2 cn)]
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
                (Length len)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: do-operation :
   String Dir Needle (U False Needle) (Listof String) (Listof Yarn) Carriers Loops String Script ->
   (values Carriers Loops Script))
(define (do-operation op d n t cs yarns carriers2 loops original2 output2)  
  ;; operation name decay based on loop presence
  (let* ([op~ (if (and (or (equal? op "knit")
                           (equal? op "split")
                           (equal? op "drop")  ;; synonym
                           (equal? op "xfer")) ;; synonym
                       (not (hash-has-key? loops n)))
                  "tuck"
                  op)]
         [b (Needle-bed n)]
         [i (Needle-index n)]
         [yarn-count (length cs)]
         [stitchsize : Positive-Flonum 30.0]) ;; TODO (JL): actually figure out how to set this (maybe using 'stitch'?)
    
    (cond
      [(equal? op~ "miss")
       (let ([output3 (miss-yarns d n cs carriers2 original2 output2)])
         (values carriers2 loops output3))]
      
      [(or (equal? op~ "amiss")
           (equal? op~ "tuck"))
       (if (zero? yarn-count)
           (values carriers2
                   loops
                   (cons (Instruction (Nop)
                                      (string-append original2 " (amiss ignored)"))
                         output2))
           (let* ([cmd (Tuck
                        (Direction d)
                        n
                        (Length stitchsize)
                        (car yarns))]
                  [n-loops (hash-ref loops n (thunk 0))]
                  [loops~ (hash-set loops n (+ n-loops
                                               yarn-count))])
             (values carriers2
                     loops~
                     (cons (Instruction cmd original2)
                           output2))))]
      
      [(or (equal? op~ "drop")
           (equal? op~ "knit"))
       (if (zero? yarn-count)
           (let ([loops~ (hash-remove loops n)])
             (values carriers2
                     loops~
                     (cons (Instruction (Drop n) original2)
                           output2)))
           (let ([cmd (Knit
                       (Direction d)
                       n
                       (Length stitchsize)
                       yarns)]
                 [loops~ (hash-set loops n yarn-count)])
             (values carriers2
                     loops~
                     (cons (Instruction cmd original2)
                           output2))))]
      
      [(or (equal? op~ "xfer")
           (equal? op~ "split"))
       (begin
         (assert (Needle? t))
         (let* ([cmd
                 (if (zero? yarn-count)
                     (Xfer n t)
                     (Split (Direction d)
                            n
                            t
                            (Length stitchsize)
                            yarns))]
                [n-loops (hash-ref loops n (thunk 0))]
                [t-loops (hash-ref loops t (thunk 0))]
                [loops1 (if (zero? yarn-count)
                            (hash-remove loops n)
                            (hash-set loops n yarn-count))]
                [loops2 (hash-set loops1 t (+ t-loops n-loops))])
           ;; update carrier attachments & parkings
           ;; (for *all* carriers -- though carriers in cs will get this info overwritten)
           (values (update-carriers b i t (hash-keys carriers2) carriers2)
                   loops2
                   (cons (Instruction cmd original2)
                         output2))))]
      
      [else (error 'fnitout "operation ~a should not be processed here" op)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; output one Miss command for each carrier specified
(: miss-yarns : Dir Needle (Listof String) Carriers String Script -> Script)
(define (miss-yarns d n p-cs carriers p-original p-output)
  (let loop ([cs       p-cs]
             [original p-original]
             [output   p-output])
    (if (null? cs)
      output
      (let* ([cn (car cs)]
             [carrier (hash-ref carriers cn)]                                                   
             [c (YarnCarrier-yarn carrier)]
             [cmd (Miss (Direction d)
                        n
                        (Carrier c))]
             [output~ (cons (Instruction cmd original)
                            output)])
        (loop (cdr cs)
                ""
                output~)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update yarn carriers parked/attached at source needle to target location
(: update-carriers : Bed Integer Needle (Listof String) Carriers -> Carriers)
(define (update-carriers b i t p-cs p-carriers)
  (let loop ([cs       p-cs]
             [carriers : Carriers p-carriers])
  (if (null? cs)
      carriers
      (let* ([cn (car cs)]
             [carrier (hash-ref carriers cn)]
             [parked-loc   (YarnCarrier-parked-loc carrier)]
             [attached-loc (YarnCarrier-attached-loc carrier)])
        (if (and (not (false? parked-loc))
                 (not (false? attached-loc)))
            (let* ([parked-loc~   (update-loc b i t parked-loc)]
                   [attached-loc~ (update-loc b i t attached-loc)]
                   [carrier~ (struct-copy YarnCarrier carrier
                                          [parked-loc   parked-loc~]
                                          [attached-loc attached-loc~])]
                   [carriers~ (hash-set carriers cn carrier~)])
              (loop (cdr cs)
                    carriers~))
            (loop (cdr cs)
                  carriers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: update-loc : Bed Integer Needle CarrierLocation -> CarrierLocation)
(define (update-loc b i t loc)
  (let* ([dir (CarrierLocation-dir loc)]
         [bed (CarrierLocation-bed loc)]
         [idx (CarrierLocation-idx loc)])
    (if (and (eq? bed b)
             (eq? idx i))
        (CarrierLocation dir
                         (Needle-bed t)
                         (Needle-index t))
        loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update locations of parked/attached yarns specified in operation 
(: update-attachments : String Dir Needle (Listof String) Carriers -> Carriers)
(define (update-attachments op d n p-cs p-carriers)
  (let* ([b (Needle-bed   n)]
         [i (Needle-index n)]
         [parked-loc~    (CarrierLocation d b i)])
    (let loop ([cs       p-cs]
               [carriers : Carriers p-carriers])
      (if (null? cs)
          carriers
          (let* ([cn (car cs)]
                 [carrier (hash-ref carriers cn)]             
                 [attached-loc  (YarnCarrier-attached-loc carrier)]
                 [attached-loc~ (if (equal? op "miss")
                                    attached-loc
                                    parked-loc~)]
                 [carrier~ (struct-copy YarnCarrier carrier
                                        [parked-loc   parked-loc~]
                                        [attached-loc attached-loc~])]
                 [carriers~ (hash-set carriers cn carrier~)])
            (loop (cdr cs)
                  carriers~))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; test
(define f (k2f "../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.k"))
(displayln (script->string f 'knitout))
;(displayln (script->string f))
;(with-output-to-file "one-fourth.f"
;  (thunk (script-export f)))
|#

;; end
