#lang racket/gui
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button)

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
        (inherit register-toolbar-button
		 register-toolbar-buttons)
        ;; receives info log messages on current-logger with topic "lsonic"
        (define receiver (make-log-receiver
                          (current-logger)
                          'info
                          'lsonic))

        (let [(play-btn
               (new switchable-button%
                    (label "Play")
                    (callback (λ (button)
                                (update-user-score (get-definitions-text)
                                                   receiver)))
                    (parent (get-button-panel))
                    (bitmap icon-play)))
              (stop-btn
               (new switchable-button%
                    (label "Stop")
                    (callback (λ (button)
                                (send-stop receiver)))
                    (parent (get-button-panel))
                    (bitmap icon-stop)))]
	  (register-toolbar-buttons (list play-btn stop-btn))
          (send (get-button-panel) change-children
                (λ (l)
                  (cons stop-btn (remq stop-btn l))))
          (send (get-button-panel) change-children
                (λ (l)
                  (cons play-btn (remq play-btn l)))))))

    (define icon-play
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc draw-text "►" 0 0)
        (send bdc set-bitmap #f)
        bmp))
    (define icon-stop
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc draw-text "█" 0 0)
        (send bdc set-bitmap #f)
        bmp))

    (define main-thread #f)
    ;; get's the thread descriptor for the main thread
    (define (get-main-thread receiver)
      (if (and main-thread (not (thread-dead? main-thread)))
          main-thread
          (begin
            (match (sync/timeout 3 receiver)
              [(vector info msg value lsonic) (set! main-thread value)]
              [#f (error 'get-main-thread "timeout getting main thread. did you press run first?")])
            main-thread)))

    ;; sends the main thread a new user score
    (define (update-user-score text receiver)
      (define uscore (string-append "(list "
                                    (filter-definitions (send text get-text))
                                    ")"))
      #;(current-output-port (open-output-file (build-path "Documents" "soniclog.txt")
                                             #:exists 'append))
      #;(printf "updating user score with\n\t~v\n" uscore)
      (thread-send (get-main-thread receiver)
                   uscore))
    ;; sends the main thread a 'stop signal
    (define (send-stop receiver)
      (thread-send (get-main-thread receiver)
                   'stop)
      (set! main-thread #f))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame sonic-pi-mixin)))

;; removes the lang line from the definitions text
;; note: could filter out comments here. was going to but the
;;       eval call takes care of it. the lang line is the only
;;       issue.
(define (filter-definitions text)
  (regexp-replace #rx"(?m:#lang.+$)\n"
                  text
                  ""))

(module+ test
  (require rackunit)
  (check-equal? (filter-definitions "#lang racket\n(run-main-method)")
                "(run-main-method)")
  (check-equal? (filter-definitions "#lang racket\n;;This is my program\n(run-main)")
                ";;This is my program\n(run-main)")
  )
