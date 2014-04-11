#lang racket
(require "irc-core.rkt"
         "plugins/auth.rkt"
         "plugins/movement.rkt"
         "plugins/irc-eval.rkt"
         "plugins/bitcoin.rkt"
         "plugins/urbandict.rkt"
         "plugins/whois.rkt")

(parameterize ([enabled-plugins (list irc-bitcoin irc-urbandict irc-movement irc-eval whois)]
               [irc-admins (set "your.hostname")])
  (irc-connect "server.address.here" 6667 "uhm" '("#channel")))
