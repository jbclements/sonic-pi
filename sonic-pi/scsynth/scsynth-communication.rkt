#lang racket/base

;; Copyright 2015-2016 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0

;; this file contains scsynth FFI. You can't use this unless you know what
;; the scsynth commands mean.

(require racket/contract
         racket/match
         racket/udp
         racket/async-channel
         osc
         "start-scsynth.rkt")

(provide
 (contract-out [comm-open (-> (list/c comm? input-port?))]
               [synchronize (-> comm? void?)]
               [send-command/elt (-> comm? osc-element? void?)]
               [send-command (->* (comm? bytes?) #:rest (listof osc-value?) void?)]
               [synchronized-command/elt (-> comm? osc-element? osc-message?)]
               [synchronized-command (->* (comm? bytes?) #:rest (listof osc-value?)
                                          osc-message?)])
 comm?)

;; a comm structure represents information necessary to communicate
;; with scsynth at the OSC level; a UDP socket, and an async-channel
;; to store incoming messages.
(define-struct comm (socket incoming) #:transparent)

(define-logger scsynth)

;; how long to wait for a response from the server, in seconds
(define SERVER-TIMEOUT 3.0)

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

;; also, UDP is not reliable, which I haven't yet thought about. For instance,
;; a synchronize should... retry if it doesn't get the response? I think
;; the idea in the audio realm is that if a particular message (note,
;; parameter change, etc.) gets lost, it's no big deal, and life goes
;; on, but you certainly don't want the whole server to lock up while
;; waiting for a sync response that got dropped.

;; create a 'comm' structure to allow communication with an existing scsynth
;; server running on port SCSYNTH-SOCKET of 127.0.0.1
(define (comm-open)
  (match-define (list scsynth-udp-socket startup-output server-stdout kill-thunk)
    (start-scsynth))
  (define the-socket (udp-open-socket))  
  (udp-bind! the-socket "127.0.0.1" 0)
  (udp-connect! the-socket "127.0.0.1" scsynth-udp-socket)
  (define incoming-messages (make-async-channel))
  (start-listening-thread! the-socket incoming-messages)
  (define the-comm (comm the-socket incoming-messages))
  ;; check for liveness, print status
  (match (synchronized-command the-comm #"/status")
    [(struct osc-message (#"/status.reply" vals))
     (match vals
       [(list _1 ugens synths groups loaded-defs avg-cpu-usage
              peak-cpu-usage nominal-framerate actual-framerate)
        (printf "server is running.\n")
        (printf " unit generators: ~v\n" ugens)
        (printf " groups: ~v\n" groups)
        (printf " loaded synth definitions: ~v\n" loaded-defs)
        (printf " average cpu usage: ~v\n" avg-cpu-usage)
        (printf " peak cpu usage: ~v\n" peak-cpu-usage)
        (printf " nominal frame rate: ~v\n" nominal-framerate)
        (printf " actual frame rate: ~v\n" actual-framerate)])]
    [other
     (error 'scsynth-communication
            "received message other than status.reply: ~a"
            other)])
  (list the-comm server-stdout))


;; start a thread that just reads incoming messages and stores
;; them in a queue.
(define (start-listening-thread! the-socket incoming-messages)
  ;; apparently, you just can't have datagrams longer than 64K bytes
  (define receive-buffer (make-bytes 65535 0))
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
       (flatten-into-queue incoming-messages decoded)
       (loop)))))


;; in principle, scsynth could return a *bundle* of
;; messages. I don't think it ever does this, but
;; it's not too hard to accommodate. We will log
;; a warning and ignore
;; the timestamp, though....
(define (flatten-into-queue incoming-messages element)
  (match element
    [(struct osc-bundle (timestamp elements))
     (log-scsynth-warning "unexpected bundle in response from scsynth")
     (for-each flatten-into-queue elements)]
    [(? osc-message? msg)
     (async-channel-put incoming-messages msg)]))

;; given an address and args, assemble a message
;; and send to server
(define (send-command ctxt address . args)
  (send-command/elt ctxt (osc-message address args)))

;; send an element (no wrapping)
(define (send-command/elt ctxt msg)
  ;; NB: THIS CAN BLOCK:
  (udp-send (comm-socket ctxt) (osc-element->bytes msg)))

;; get a message (but don't wait longer than SERVER-TIMEOUT
(define (wait-for-message incoming-messages)
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
(define (discard-til-match incoming-messages pred)
  (define msg (wait-for-message incoming-messages))
  (cond [(pred msg) ; success!
         (void)]
        [else (log-scsynth-warning
               "discarding message while waiting for a different one: ~e"
               msg)
              (discard-til-match incoming-messages pred)]))

;; use /sync to discard all waiting messages and synchronize.
;; might take time... but not too much.
(define (synchronize ctxt)
  (define sync-id (fresh-sync-id))
  (send-command ctxt #"/sync" sync-id)
  (discard-til-match (comm-incoming ctxt)
                     (lambda (msg)
                       (and (equal? (osc-message-address msg) #"/synced")
                            (equal? (osc-message-args msg) (list sync-id))))))

;; use /sync to discard all waiting messages, then make the
;; call and wait for an incoming one. Combines address and
;; args into a single message
(define (synchronized-command ctxt address . args)
  (synchronized-command/elt ctxt (osc-message address args)))

;; use /sync to discard all waiting messages, then make the
;; call and wait for an incoming one.
(define (synchronized-command/elt ctxt msg)
  (synchronize ctxt)
  (send-command/elt ctxt msg)
  (wait-for-message (comm-incoming ctxt)))

