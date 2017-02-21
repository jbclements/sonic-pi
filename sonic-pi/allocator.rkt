#lang racket

(require osc)

;; provides simple allocation functions
;; that are needed in a few places
(provide fresh-sync-id
         fresh-node-id!
         fresh-bus-id
         current-outbus
         set-current-outbus)

;; each sync command should use a unique # per connection
(define SYNC-ID (box 23))
(define (fresh-sync-id)
  (set-box! SYNC-ID (add1 (unbox SYNC-ID)))
  (sub1 (unbox SYNC-ID)))

;; create a fresh node id.
(define fresh-node-id!
  ;; each node must have a unique ID.
  ;; Worrying about overflow is probably silly....
  (let ([node-id (box 2)])
    (λ ()
      (cond [(int32? (unbox node-id))
             (set-box! node-id (add1 (unbox node-id)))
             (sub1 (unbox node-id))]
            [else (error 'node-id "current node id too large: ~v\n"
                         (unbox node-id))]))))

;; create a new bus for sound fx
(define OUT-BUS (box 12))
(define (fresh-bus-id)
  (set-box! OUT-BUS (+ 2 (unbox OUT-BUS)))
  (unbox OUT-BUS))

;; current out bus
(define CURR-OUT-BUS (box 12))
(define (current-outbus) (unbox CURR-OUT-BUS))
(define (set-current-outbus bus) (set-box! CURR-OUT-BUS bus))



