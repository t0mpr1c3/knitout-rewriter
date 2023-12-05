#lang racket

;; https://doi.org/10.1145/3592449

(provide (struct-out Direction)
         (struct-out Needle)
         (struct-out Carrier)
         (struct-out Length)
         (struct-out Yarn)
         (struct-out Tuck)
         (struct-out Op)
         (struct-out OpN)
         (struct-out OpNT)
         (struct-out OpND)
         (struct-out OpNDC)
         (struct-out OpNDL)
         (struct-out OpNDLY)
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
         op/c
         opn/c
         opnt/c
         opnd/c
         opndc/c
         opndl/c
         opndly/c
         rack/c
         tuck/c
         split/c)

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

;; Base command types

(struct Op
  (comment)
  #:prefab)

(define op/c
  (struct/dc Op
             [comment string?]))

(struct OpN Op
  (needle)
  #:prefab)

(define opn/c
  (struct/dc OpN
             [(comment #:parent Op) string?]             
             [needle needle/c]))

(struct OpNT OpN
  (target)
  #:prefab)

(define opnt/c
  (struct/dc OpNT
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c]             
             [target needle/c]))

(struct OpND OpN
  (direction)
  #:prefab)

(define opnd/c
  (struct/dc OpND
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [direction direction/c]))

(struct OpNDC OpND
  (carrier)
  #:prefab)

(define opndc/c
  (struct/dc OpNDC
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [(direction #:parent OpND) direction/c]
             [carrier carrier/c]))

(struct OpNDL OpND
  (length)
  #:prefab)

(define opndl/c
  (struct/dc OpNDL
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [(direction #:parent OpND) direction/c]
             [length length/c]))

(struct OpNDLY OpNDL
  (yarns)
  #:prefab)

(define opndly/c
  (struct/dc OpNDLY
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [(direction #:parent OpND) direction/c]
             [(length #:parent OpNDL) length/c]
             [yarns (listof yarn/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nop struct
(struct Nop Op
  ()
  #:prefab)

;; Rack struct
(struct Rack Op
  (racking)
  #:prefab)

(define rack/c
  (struct/dc Rack             
             [(comment #:parent Op) string?]
             [racking integer?]))

;; Drop struct
(struct Drop OpN
  ()
  #:prefab)

;; Xfer struct
(struct Xfer OpNT
  ()
  #:prefab)

;; Miss struct
(struct Miss OpNDC
  ()
  #:prefab)

;; In struct
(struct In OpNDC
  ()
  #:prefab)

;; Out struct
(struct Out OpNDC
  ()
  #:prefab)

;; Knit struct
(struct Knit OpNDLY
  ()
  #:prefab)

;; Split struct
(struct Split OpNDLY
  (target)
  #:prefab)

(define split/c
  (struct/dc Split
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [(direction #:parent OpND) direction/c]
             [(length #:parent OpNDL) length/c]
             [(yarns #:parent OpNDLY) (listof yarn/c)]
             [target needle/c]))

;; Tuck struct
(struct Tuck OpNDL
  (yarn)
  #:prefab)

(define tuck/c
  (struct/dc Tuck
             [(comment #:parent Op) string?]             
             [(needle #:parent OpN) needle/c] 
             [(direction #:parent OpND) direction/c]
             [(length #:parent OpNDL) length/c]
             [yarn yarn/c]))

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