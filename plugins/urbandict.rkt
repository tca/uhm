#lang racket

(require json
         net/url
         net/uri-codec
         "../irclib.rkt")

(provide irc-urbandict)

(define base-url "http://api.urbandictionary.com/v0/define?term=")

(define (get-definition-json term)
  (let ([url (string-append base-url (uri-encode term))])
    (call/input-url (string->url url) get-pure-port port->string)))

(define (get-definition term)
  (let ([json (string->jsexpr (get-definition-json term))])
    (hash-ref (first (hash-ref json 'list)) 'definition)))

(define (handle-msg msg)
  (match msg
    [(irc-msg (irc-user nick _ _) "PRIVMSG" chan (pregexp "^;ud (.+)" (list _ term)))
       (irc-privmsg chan (format "~a: ~a" nick (get-definition term)))]
    [_ '()]))

(define (loop)
  (handle-msg (thread-receive))
  (loop))

(define (irc-urbandict)
  (thread loop))
