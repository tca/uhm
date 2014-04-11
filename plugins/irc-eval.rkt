#lang racket

(require "../irclib.rkt" racket/sandbox)
(provide irc-eval)

(define (normalize-mv . vs)
  (match vs
    [(cons v '()) v]
    [_ (string-join vs ", ")]))

(define (safe-eval exp)
  (with-handlers ([(λ (_) #t) (λ (e) (format "Command aborted: ~a" (exn-message e)))])
    (parameterize ([sandbox-eval-limits '(5 5)])
      (call-with-values (λ () ((make-evaluator 'racket '(require racket/control)) exp)) normalize-mv))))

(define (handle-msg msg)
  (match msg
    [(irc-msg (irc-user nick _ _) "PRIVMSG" chan (pregexp "^;eval(.+)" (list _ code)))
     (irc-privmsg chan (format "~a: ~a" nick (safe-eval code)))]
    [_ '()]))

(define (loop)
  (handle-msg (thread-receive))
  (loop))

(define (irc-eval)
  (thread loop))
