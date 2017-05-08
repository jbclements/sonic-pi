#lang info

(define name "Sonic PI")

(define scribblings '(("sonic-pi.scrbl" () (tool))))

(define compile-omit-paths '())
(define test-omit-paths '("examples"))

;; Defining the Sonic-Pi run button
(define drracket-tools      `("tool.rkt"))
(define drracket-tool-names `("Sonic-Pi Button"))
(define drracket-tool-icons `("icon.png"))