#lang racket/gui
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         "../sonic-pi/sonic-pi/lsonic.rkt")

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
  
    (define sonic-pi-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-interactions-text
                 get-definitions-text)
        (inherit register-toolbar-button)
  
        (let ((btn
               (new switchable-button%
                    (label "Sonic Pi")
                    (callback (λ (button)
                                (if (pressed?)
                                    (error 'button "YASSS")
                                    (begin
                                      (set-pressed)
                                      (run-scsynth (get-definitions-text)
                                                 (get-interactions-text))))))
                    (parent (get-button-panel))
                    (bitmap icon))))
          (register-toolbar-button btn)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))

    ;; let's define a box to keep track of whether the button was pressed once already
    (define pressed (box #f))
    (define (pressed?) (unbox pressed))
    (define (set-pressed) (set-box! pressed #t))
  
    (define icon
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc draw-text "♫" 0 0)
        ;(send bdc draw-ellipse 2 2 8 8)
        ;(send bdc set-brush "red" 'solid)
        ;(send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))
    
    (define (run-scsynth text int)
      (begin (random-seed 52)
             (current-output-port (open-output-file (build-path "."
                                                                "soniclog.txt")))
             (define ctxt (startup))
             (define job-ctxt (start-job ctxt))
             (define language (first (drracket:language-configuration:get-languages)))
             (with-handlers
                 ([exn:fail? (lambda (exn)
                               (printf "ending job due to error...\n")
                               (end-job job-ctxt)
                               (raise exn))])               
               #;(string-split (send text get-text) "\n")
               
               (play job-ctxt (l-eval (string-append
                                       "(list"
                                       (string-append* (rest (string-split (send text get-text) "\n")))
                                       ")")))
               (sleep 15)
               (end-job job-ctxt))))
    
    (define (phase1) (void))
    (define (phase2) (void))
  
    (drracket:get/extend:extend-unit-frame sonic-pi-mixin)))