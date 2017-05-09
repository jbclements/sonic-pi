#lang racket

(provide (all-defined-out))

;; a loop is a number representing the number of reps and
;; a block, which is a closure over a user score
(struct Loop (reps block))
;; a live_loop is a name and a block containing a user score
(struct Live_Loop (name block))

;; create a loop structure, given a number of reps and a block 
(define (loop reps block)
  (Loop reps block))
;; create new loop where the number of reps is one less than the one supplied
(define (sub1loop lp)
  (Loop (sub1 (Loop-reps lp))
        (Loop-block lp)))
;; create a live_loop structure with a name
(define (live_loop name block)
  (Live_Loop name block))

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
  (apply thread-wait (hash-values thread-buffer)))
;; kills all threads immediately
(define (kill-all-threads)
  (apply kill-thread (hash-values thread-buffer)))