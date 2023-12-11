#lang typed/racket

;; https://doi.org/10.1145/3592449

(require "../fnitout-command.rkt"
         "../fnitout-validator.rkt")

(require/typed "../fnitout-parser.rkt"
               [fnitout-parse (-> String (Listof (Pairof Command String)))])

;(module+ test
;  (require typed/rackunit)

  (define script
    (map (λ ([x : (Pairof Command String)])
            (Instruction (car x) (cdr x)))
    (fnitout-parse
     (port->string
      (open-input-file
       ;"../../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.f"))))
       "../../fenced-tangle-supplemental/examples/pleat-tube/two-thirds.f")))))
  (define validated (validate (make-Validator 250 10) script))

  (for ([x : Record
           (in-list
            (filter-not
             (λ ([x : Record])
               (null? (Record-error x)))
             validated))])
    (writeln x))

  ;; end of submodule
;  )

;; end

