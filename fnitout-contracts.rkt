#lang racket

;; https://doi.org/10.1145/3592449

(provide (struct-out Direction)
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
         (struct-out Nop)
         (contract-out
          [opposite-dir    (-> direction/c dir?)]
          [parse-racking   (-> syntax? integer?)]
          [parse-direction (-> syntax? direction/c)]
          [parse-needle    (-> syntax? needle/c)]
          [parse-length    (-> syntax? length/c)]
          [parse-carrier   (-> syntax? carrier/c)]
          [parse-yarn      (-> syntax? yarn/c)])
         interval/c
         extent/c
         tuck/c
         knit/c
         split/c
         miss/c
         in/c
         out/c
         drop/c
         xfer/c
         rack/c
         nop/c)

(require racket/contract
         racket/syntax
         syntax/parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Direction struct
(struct Direction
  (val)
  #:prefab)

(define dir?
  (flat-named-contract
   'dir?
   (lambda (x)
     (and (symbol? x)
          (or (eq? '+ x)
              (eq? '- x))))))

(define direction/c
  (struct/dc Direction
             [val dir?]))

(define (opposite-dir direction)
  (if (eq? '+ (Direction-val direction)) '- '+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Needle struct
(struct Needle
  (bed index)
  #:prefab)

(define bed?
  (flat-named-contract
   'bed?
   (lambda (x)
     (and (symbol? x)
          (or (eq? 'f x)
              (eq? 'b x))))))

(define needle/c
  (struct/dc Needle
             [bed bed?]
             [index integer?])) ;; FIXME set boundaries on needle index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Carrier struct
(struct Carrier
  (val)
  #:prefab)

(define carrier/c
  (struct/dc Carrier
             [val (and/c positive? integer?)])) ;; FIXME set maximum for carrier index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Length struct
(struct Length
  (val)
  #:prefab)

(define length/c
  (struct/dc Length
             [val (and/c rational? positive?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yarn struct
(struct Yarn
  (carrier length)
  #:prefab)

(define yarn/c
  (struct/dc Yarn
             [carrier carrier/c]
             [length length/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interval struct
(struct Interval
  (min max)
  #:transparent
  #:guard
  (λ (min max name)
    (when (> min max)
      (error 'fnitout "invalid values for interval"))
    (values min max)))

(define interval/c
  (struct/dc Interval
             [min real?]
             [max real?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extent struct
(struct Extent
  (x y)
  #:transparent)

(define extent/c
  (struct/dc Extent
             [x interval/c]
             [y interval/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tuck struct
(struct Tuck
  (direction needle length yarn)
  #:prefab)

(define tuck/c
  (struct/dc Tuck
             [direction direction/c]
             [needle needle/c]
             [length length/c]
             [yarn yarn/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Knit struct
(struct Knit
  (direction needle length yarns)
  #:prefab)

(define knit/c
  (struct/dc Knit
             [direction direction/c]
             [needle needle/c]
             [length length/c]
             [yarns (listof yarn/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Split struct
(struct Split
  (direction needle target length yarns)
  #:prefab)

(define split/c
  (struct/dc Split
             [direction direction/c]
             [needle needle/c]
             [target needle/c]
             [length (or/c length/c #f)]
             [yarns (listof yarn/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Miss struct
(struct Miss
  (direction needle carrier)
  #:prefab)

(define miss/c
  (struct/dc Miss
             [direction direction/c]
             [needle needle/c]
             [carrier carrier/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In struct
(struct In
  (direction needle carrier)
  #:prefab)

(define in/c
  (struct/dc In
             [direction direction/c]
             [needle needle/c]
             [carrier carrier/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Out struct
(struct Out
  (direction needle carrier)
  #:prefab)

(define out/c
  (struct/dc Out
             [direction direction/c]
             [needle needle/c]
             [carrier carrier/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Drop struct
(struct Drop
  (needle)
  #:prefab)

(define drop/c
  (struct/dc Drop
             [needle needle/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Xfer struct
(struct Xfer
  (needle target)
  #:prefab)

(define xfer/c
  (struct/dc Xfer
             [needle needle/c]
             [target needle/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rack struct
(struct Rack
  (racking)
  #:prefab)

(define rack/c
  (struct/dc Rack
             [racking integer?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nop struct
(struct Nop
  ()
  #:prefab)

(define nop/c
  (struct/dc Nop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parsing macros for command parameters

(define (parse-racking rack-stx)
  (syntax-parse rack-stx
    [(_ r-stx)
     (syntax->datum #'r-stx)]))

(define (parse-direction direction-stx)
  (syntax-parse direction-stx
    [(_ dir-stx)
     (Direction (string->symbol (syntax->datum #'dir-stx)))]))

(define (parse-needle needle-stx)
  (syntax-parse needle-stx
    [(_ bed-stx index-stx)
     (Needle (string->symbol (syntax->datum #'bed-stx))
             (cadr (syntax->datum #'index-stx)))]))

(define (parse-carrier carrier-stx)
  (syntax-parse carrier-stx
    [(_ carrier-stx)
     (Carrier (cadr (syntax->datum #'carrier-stx)))]))

(define (parse-length length-stx)
  (syntax-parse length-stx
    [(_ size-stx)
     (Length (cadr (syntax->datum #'size-stx)))]))

(define (parse-yarn yarn-stx)
  (syntax-parse yarn-stx
    [(_ carrier-stx length-stx)
     (Yarn (parse-carrier #'carrier-stx)
           (parse-length #'length-stx))]))

;; end