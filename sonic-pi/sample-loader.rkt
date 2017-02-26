#lang racket

;; this file handles all the sample loading behavior
;; which includes a hash buffer for storing the
;; information about loaded samples
(require "scsynth/scsynth-abstraction.rkt"
         "scsynth/scsynth-communication.rkt"
         osc)

(provide load-sample
         sample-loaded?)

(define sample-buffer (make-hash))

;; load a sample into a buffer, then save the
;; information about the buffer
(define (load-sample job-ctxt spath)
  ; get fresh buffer id
  (define buff-id (fresh-buffer-id!))
  (define the-comm (ctxt-comm (job-ctxt-ctxt job-ctxt)))
  ; send message to allocate and read sample file
  (send-command/elt
   the-comm
   (osc-message
    #"/b_allocRead"
    (append (list buff-id spath 0 0))))
  ; sync
  (wait-for-buffer the-comm)
  ;(synchronize the-comm)
  ; verify buffer load was a success
  (define b-info (query-buffer the-comm buff-id))
  ; add buffer-info to the hash
  (hash-set! sample-buffer spath b-info)
  ; return the buffer-info
  b-info)

;; determine if a sample is already loaded
(define (sample-loaded? spath)
  (hash-ref sample-buffer spath (λ () #false)))

;; load samples
(define (load-samples job-ctxt spaths)
  (apply (λ (path) (load-sample job-ctxt path))
         spaths))


;; create a new buffer id
(define fresh-buffer-id!
  (let ([buff-id (box 1)])
    (λ ()
      (cond [(int32? (unbox buff-id))
             (set-box! buff-id (add1 (unbox buff-id)))
             (sub1 (unbox buff-id))]
            [else (error 'buff-id "ran out of buffer space")]))))