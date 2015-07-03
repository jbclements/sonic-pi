#lang racket

;; Copyright 2015 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0


(require racket/udp
         racket/runtime-path
         racket/async-channel
         osc)

(define-runtime-path here ".")

#;(define setup-messages (file->value (build-path here "preloads.rktd")))
#;(define action-messages (file->value (build-path here "actions.rktd")))

(define scsynth-socket 57118)
(define receive-socket 57119)

(define the-socket (udp-open-socket))

(udp-bind! the-socket "127.0.0.1" receive-socket)

;; this assumes you've already started scsynth, like this:
;"scsynth -u 57118"

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
     (async-channel-put incoming-messages decoded)
     (loop))))

(define (send-command message)
  (udp-send-to the-socket "127.0.0.1" scsynth-socket 
               (osc-element->bytes message)))

;; check for status message
;; initially, assume that this is the first message. Will this be true?
;; not sure.
(send-command (osc-message #"/status" empty))

;; how long to wait for a response from the server
(define SERVER-TIMEOUT 3.0)

(match (sync/timeout SERVER-TIMEOUT incoming-messages)
  [#f (error 'scsynth-communication
             "expected response to status message in ~a seconds"
             SERVER-TIMEOUT)]
  [(struct osc-message (#"/status.reply" vals))
   (printf "yay, got status reply with values: ~a\n"
           vals)]
  [other
   (error 'scsynth-communication
          "received message other than status.reply: ~a"
          other)])


#;(
(for ([msg setup-messages])
  (send-command msg))


;; start the sine wave:
(send-command (first action-messages))

;; wait, so that the printf winds up below all of the other printout:
(sleep 0.5)
(printf "hit return to stop.\n")
(read-line)

(for ([msg (rest action-messages)])
  (send-command msg))

;; sleep so that we can receive the rest of the response messages
(sleep 0.5))





