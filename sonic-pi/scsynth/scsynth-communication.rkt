#lang racket

;; Copyright 2015 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0

(require racket/udp
         racket/runtime-path
         racket/async-channel
         osc)

(provide send-command
         send-command/msg
         synchronized-command
         synchronized-command/msg
         synchronize
         incoming-messages)

(define-logger scsynth)

;; don't test this file...
(module test racket/base)

(define SCSYNTH-SOCKET 57118)
(define RECEIVE-SOCKET 57119)

(define the-socket (udp-open-socket))

(udp-bind! the-socket "127.0.0.1" RECEIVE-SOCKET)

;; this assumes you've already started scsynth, like this:
;"scsynth -u 57118"
;; from sonic pi.  Maybe need -m argument for real-time memory setting? and -a argument
;; for number of audio buses?
;; sys("'#{scsynth_path}' -a 1024 -u #{@port} -m 131072 -D 0 &")
;; ouch, there's other stuff that's somehow necessary, hiding in relative paths?
;; this one works:
;; /Applications/Sonic\ Pi.app/app/server/native/osx/scsynth -a 1024 -u 57118 -m 131072 -D 0

;; apparently, you just can't have datagrams longer than 64K bytes
(define receive-buffer (make-bytes 65535 0))

;; NB: what I know about scsynth OSC messages comes from the
;; "Server Command Reference" HTML document that's a part of this repo.

;; some messages trigger response messages. However, matching responses
;; with messages is a little tricky.  Some messages (e.g. "/dumpOSC")
;; have no response at all, so there's no matching to do. Other messages
;; do have responses. For instance, /s_get gets a control value, and
;; the response message is an /n_set. But what if you have multiple messages
;; with responses that might be confused with each other, and might arrive
;; out of order? In this case, you need to use the /sync command, which
;; accepts a unique id and responds with /synced and the corresponding id
;; when all of the commands preceding the /sync are done.

;; for now, I'm going to launch a thread that just stuffs all of the
;; incoming messages into an async-channel.

(define incoming-messages
  (make-async-channel))

;; start a thread that just reads incoming messages and stores
;; them in a queue.

(thread
 (lambda ()
   (let loop ()
     (define-values (len hostname src-port)
       (udp-receive! the-socket receive-buffer))
     #;(printf "len: ~v\nhostname: ~v\nsrc-port: ~v\n" len hostname src-port)
     (define received (subbytes receive-buffer 0 len))
     #;(printf "received buffer: ~v\n" received)
     (define decoded (bytes->osc-element received))
     #;(printf "decoded: ~e\n" decoded)
     (flatten-for-queue decoded)
     (loop))))

;; in principle, scsynth could return a *bundle* of
;; messages. I don't think it ever does this, but
;; it's not too hard to accommodate. We will log
;; a warning and ignore
;; the timestamp, though....
(define (flatten-for-queue element)
  (match element
    [(struct osc-bundle (timestamp elements))
     (log-scsynth-warning "unexpected bundle in response from scsynth")
     (for-each flatten-for-queue elements)]
    [(? osc-message? msg)
     (async-channel-put incoming-messages msg)]))

;; given an address and args, assemble a message
;; and send to server
(define (send-command address . args)
  (send-command/msg (osc-message address args)))

;; send a message (no wrapping)
(define (send-command/msg msg)
  (udp-send-to the-socket "127.0.0.1" SCSYNTH-SOCKET
               (osc-element->bytes msg)))

;; how long to wait for a response from the server
(define SERVER-TIMEOUT 3.0)

;; get a message (but don't wait longer than SERVER-TIMEOUT
(define (wait-for-message)
  (match (sync/timeout SERVER-TIMEOUT incoming-messages)
    [#f (error 'scsynth-communication
               "expected scsynth message in < ~a seconds"
               SERVER-TIMEOUT)]
    [msg msg]))


;; each sync command should use a unique # per connection
(define SYNC-ID (box 23))
(define (fresh-sync-id)
  (set-box! SYNC-ID (add1 (unbox SYNC-ID)))
  (sub1 (unbox SYNC-ID)))

;; discard messages until you get one that matches the
;; predicate. Don't wait longer than SERVER-TIMEOUT
;; for any of them.
(define (discard-til-match pred)
  (define msg (wait-for-message))
  (cond [(pred msg) 'done]
        [else (log-scsynth-warning
               "discarding message while waiting for a different one: ~e"
               msg)
              (discard-til-match pred)]))

;; use /sync to discard all waiting messages and synchronize.
;; might take time... but not too much.
(define (synchronize)
  (define sync-id (fresh-sync-id))
  (send-command #"/sync" sync-id)
  (discard-til-match (lambda (msg)
                       (and (equal? (osc-message-address msg) #"/synced")
                            (equal? (osc-message-args msg) (list sync-id))))))

;; use /sync to discard all waiting messages, then make the
;; call and wait for an incoming one. Combines address and
;; args into a single message
(define (synchronized-command address . args)
  (synchronized-command/msg (osc-message address args)))

;; use /sync to discard all waiting messages, then make the
;; call and wait for an incoming one.
(define (synchronized-command/msg msg)
  (synchronize)
  (send-command/msg msg)
  (wait-for-message))


;; check for liveness
(match (synchronized-command #"/status")
  [(struct osc-message (#"/status.reply" vals))
   (printf "yay, got status reply with values: ~a\n"
           vals)]
  [other
   (error 'scsynth-communication
          "received message other than status.reply: ~a"
          other)])