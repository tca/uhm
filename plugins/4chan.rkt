#lang racket

(require json
         net/url
         srfi/13
         "../irclib.rkt")

(provide irc-4chan)

(define (get-catalog board)
  (let ([url (format "http://a.4cdn.org/~a/catalog.json" board)])
    (string->jsexpr (call/input-url (string->url url) get-pure-port port->string))))
  
(define (search-board-topics board-data term)
  (let* ([pages board-data]
         [threads (append-map (lambda (p) (hash-ref p 'threads)) pages)]
         [filtered (filter (lambda (t) (string-contains-ci (hash-ref t 'com "") term)) threads)]
         [ids (map (lambda (t) (hash-ref t 'no)) filtered)])
    ids))

(define (search-topics board term)
  (string-join 
   (map (lambda (no) (format "https://boards.4chan.org/~a/res/~a" board no))
        (search-board-topics (get-catalog board) term))
    " "))

(define (handle-msg msg)
  (match msg
    [(irc-msg (irc-user nick _ _) "PRIVMSG" chan (pregexp "^;topic /?([a-zA-Z0-9]+)/? (.+).$" (list _ board term)))
     (irc-privmsg chan (format "~a: ~a" nick (search-topics board term)))]
    [_ '()]))

(define (loop)
  (handle-msg (thread-receive))
  (loop))

(define (irc-4chan)
  (thread loop))