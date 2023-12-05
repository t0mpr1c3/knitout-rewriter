#lang typed/racket

;; https://doi.org/10.1145/3592449

(require "../fnitout-validater.rkt")

(module+ test
  (require typed/rackunit)

  (check-not-exn
   (λ ()
     (define demo
       (port->string
        (open-input-file
         "../../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.f")))
     (validate (make-Validater 250 10 demo))))

  ;; end of submodule
  )

;; end