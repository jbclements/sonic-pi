#lang racket

;; this file handles all the sample loading behavior
;; which includes a hash buffer for storing the
;; information about loaded samples
(require "scsynth-abstraction.rkt"
         "scsynth-communication.rkt"
         osc)

(provide load-sample
         load-samples
         sample-loaded?)

;; store buffer information in a hash
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

;; load a list of samples in advance
(define (load-samples job-ctxt spaths)
  (map (λ (path) (load-sample job-ctxt path))
         spaths))

;; creates a new sample buffer id.
;; works like node ids
(define fresh-buffer-id!
  (let ([buff-id (box 1)])
    (λ ()
      (cond [(int32? (unbox buff-id))
             (set-box! buff-id (add1 (unbox buff-id)))
             (sub1 (unbox buff-id))]
            [else (error 'buff-id "ran out of buffer space")]))))


(module+ test
  (require "../sample.rkt")
  (require rackunit)
  (define ctxt (startup))
  (define job-ctxt (start-job ctxt))
  (define b-infos (load-samples job-ctxt (list (sample-path (sample "ambi_choir"))
                               (sample-path (sample "bd_tek")))))
  ;; because sample-loaded? returns the b-info I had to get that manually
  (check-equal? (sample-loaded? (sample-path (sample "ambi_choir")))
                (first b-infos))
  (check-equal? (sample-loaded? (sample-path (sample "bd_tek")))
                (second b-infos))
  (check-false (sample-loaded? (sample-path (sample "cnoise"))))
  (sleep 5)
  (end-job job-ctxt))