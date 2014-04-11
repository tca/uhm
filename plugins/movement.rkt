#lang racket

(require "../irclib.rkt" "auth.rkt")
(provide irc-movement)

(define (handle-msg msg)
  (match msg
    [(irc-msg (? admin?) "PRIVMSG" chan msg-txt)
     (match msg-txt
       [(pregexp "^;do (.+)" (list _ cmd)) (irc-out cmd)]
       [(pregexp "^;part (.+)" (list _ chan)) (irc-part-channel chan)]
       [(pregexp "^;join (.+)" (list _ chan)) (irc-join-channel chan)]
       [(pregexp "^;quit") (irc-quit) (exit)]
       [_ '()])]
    [_ '()]))

(define (loop)
  (handle-msg (thread-receive))
  (loop))

(define (irc-movement)
  (thread loop))
