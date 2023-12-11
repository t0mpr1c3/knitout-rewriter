#lang typed/racket

;; https://textiles-lab.github.io/knitout/knitout.html

(provide script->knitout)

(require threading)
(require "fnitout-command.rkt"
         "fnitout-serialization.rkt"
         "k2f.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expands operations, removes explicit kickbacks, combines successive rack commands
;; operates on reversed script
(: script->knitout : Script -> Script)
(define (script->knitout self)
  (let ([expanded (script-expand self)])
    (let loop ([xs : (Listof (Pairof Command String)) (reverse expanded)]
               [a  : (Listof (Pairof Command String)) null])
      (if (null? xs)
          (map (λ ([x : (Pairof Command String)])
                 (Instruction (car x) (cdr x)))
               a)
          (let* ([x   (car xs)]
                 [cmd (car x)])
            (if (Rack? cmd)
                (let-values ([(xs~ comment~)
                              (skip-rack-cmds (cdr xs) (cdr x))])
                  (loop xs~
                        (cons (cons cmd comment~) ;; use comment from last Rack
                              a)))
                (if (or (Tuck?  cmd)
                        (Knit?  cmd)
                        (Split? cmd)
                        (Miss?  cmd))
                    (let-values ([(xs~ comment~)
                                  (skip-miss-cmds (cdr xs) (command-carriers cmd) (cdr x))])
                      (loop xs~
                            (cons (cons cmd comment~) ;; use comment from last Miss
                                  a)))
                    (loop (cdr xs)
                          (cons x a)))))))))

;; arguably this procedure is unnecessary
(: skip-rack-cmds : (Listof (Pairof Command String)) String -> (values (Listof (Pairof Command String)) String))
(define (skip-rack-cmds p-xs p-comment)
  (let loop ([xs      p-xs]
             [comment p-comment])
    (if (null? xs)
        (values null comment)
        (let ([cmd (caar xs)])
          (if (or (Nop? cmd)
                  (Rack? cmd))
              (loop (cdr xs) (cdar xs))
              (values xs comment))))))

;; arguably this procedure is unnecessary
(: skip-miss-cmds : (Listof (Pairof Command String)) (Listof Carrier) String -> (values (Listof (Pairof Command String)) String))
(define (skip-miss-cmds p-xs p-cs p-comment)
  (let loop ([xs      p-xs]
             [cs     (list->set p-cs)]
             [comment p-comment])
    (if (null? xs)
        (values null comment)
        (let ([cmd (caar xs)])
          (if (or (Nop? cmd)
                  (and (Miss? cmd)
                       (set-member? cs (Miss-carrier cmd))))
              (loop (cdr xs) cs (cdar xs))
              (values xs comment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test
#|
(define f (k2f "../fenced-tangle-supplemental/examples/pleat-tube/one-fourth.k"))
(displayln (script->string (script->knitout f) 'knitout))
|#
(define f (k2f "../fenced-tangle-supplemental/examples/pleat-tube/two-thirds.k"))
(displayln (script->string f 'fnitout))
(newline)
(displayln (script->string (script->knitout f) 'knitout))

;; end
