#lang racket

(provide (all-defined-out))


;; we need a global for threads in the same way we have
;; one for samples
(define thread-buffer (make-hash))

;; determines if a thread already exists in the buffer
(define (thread-exists? name)
  (hash-ref thread-buffer name (λ () #false)))

;; adds named thread to the buffer
(define (save-thread name desc)
  (hash-set! thread-buffer name desc))

;; gets the thread descriptor from the name
;; works the same as thread-exists?
(define get-thread-by-name thread-exists?)

;; waits on all threads to finish executing
(define (wait-on-all-threads)
  (map thread-wait (hash-values thread-buffer)))

;; kills all threads immediately
(define (kill-all-threads)
  (map kill-thread (hash-values thread-buffer)))

;; all threads are dead
(define (all-threads-dead?)
  (andmap thread-dead? (hash-values thread-buffer)))