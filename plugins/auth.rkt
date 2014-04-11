#lang racket

(require "../irclib.rkt")
(provide irc-admins admin?)

(define irc-admins (make-parameter (set)))
(define (admin? user) (and (irc-user? user) (set-member? (irc-admins) (irc-user-host user))))
