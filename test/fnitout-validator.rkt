#lang typed/racket

;; https://doi.org/10.1145/3592449

(require "../fnitout-command.rkt"
         "../fnitout-validator.rkt")

(require/typed "../fnitout-parser.rkt"
               [fnitout-parse (-> String (Listof Command))])

(module+ test
  (require typed/rackunit)

  (check-not-exn
   (λ ()
     (define demo
       (fnitout-parse
        (port->string
         (open-input-file
          ;"../../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.f"))))
          "../../fenced-tangle-supplemental/examples/pleat-tube/two-thirds.f"))))
     (validate (make-Validator 250 10) demo)))

  ;; end of submodule
  )

;; end

