#lang racket

(require "irclib.rkt")
(provide enabled-plugins irc-connect)

(define enabled-plugins (make-parameter '()))
(define running-plugins '())

(define (irc-out-loop out)
  (let* ([msg (thread-receive)]
         [msg-coerced (~a msg)])
    (when (valid-message-length? msg-coerced)
      (fprintf out (irc-clean-msg msg-coerced))
      (printf "> ~a\n" msg-coerced))
    (irc-out-loop out)))

(define (handle-msg msg)
  (for-each (λ (t) (thread-send t msg)) running-plugins))

(define (irc-in-loop in)
  (let ([msg (read-line in)])
    (displayln msg)
    (cond [(irc-parse-ping msg) => irc-pong]
	  [(irc-parse-msg msg) => handle-msg]
	  [else (displayln "unhandled input 1")])
    (irc-in-loop in)))

(define (register-plugin! plugin)
  (set! running-plugins (cons plugin running-plugins)))

(define (load-plugin! plugin)
  (cond [(plugin) => register-plugin!]
	[else (displayln "failed to start plugin")]))

(define (irc-connect address port nick chans)
  (let-values ([(in out) (tcp-connect address port)])
    (file-stream-buffer-mode out 'line)
    (parameterize ([irc-out-thread (thread (λ () (irc-out-loop out)))])
      (for-each load-plugin! (enabled-plugins))
      (irc-declare-nick nick)
      (irc-declare-user nick)
      (for-each irc-join-channel chans)
      (irc-in-loop in))))
