#lang racket

(provide
 irc-out-thread
 irc-user irc-user-host irc-user-nick irc-user-user irc-user?
 irc-server irc-server-host irc-msg irc-msg-user irc-msg-action irc-msg-args irc-msg-tail
 irc-declare-nick irc-declare-user
 irc-pong irc-out irc-privmsg irc-whois irc-whowas
 irc-join-channel irc-part-channel irc-quit
 valid-message-length? irc-clean-msg irc-parse-ping
 irc-parse-msg)


;; Message Types

(struct irc-user (nick user host))
(struct irc-server (host))
(struct irc-msg (user action args tail))


;; Message parsing

(define re-ping #px"^PING :(.+)")
(define re-nick "((?i:[a-z][a-z0-9\\-\\[\\]\\\\`\\^\\{\\}_]*))")
(define re-user "(\\S[^@]*)")
(define re-hostname "([a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*)")
(define re-user-prefix (string-append re-nick "!" re-user "@" re-hostname))
(define re-prefix (pregexp (format "^:(?:(?:~a)|(?:~a)) (.*)" re-user-prefix re-hostname)))
(define re-command "^(\\d{3}|[A-Z]+)")
(define re-params "(?: ([^\\s:]+))*")
(define re-trail "(?: :(.*))?")
(define re-rest (pregexp (string-append re-command re-params re-trail)))

(define (irc-parse-prefix msg)
  (match (regexp-match re-prefix msg)
    [(list _ #f #f #f host rest) (values (irc-server host) rest)]
    [(list _ nick user host #f rest) (values (irc-user nick user host) rest)]
    [_ (values #f #f)]))

(define (irc-parse-rest msg-rest)
  (match (regexp-match re-rest msg-rest)
    [(list _ action args tail) (values action args tail)]
    [_ (values #f #f #f)]))

(define (irc-parse-msg msg)
  (let-values ([(user tail) (irc-parse-prefix msg)])
    (if (or user tail)
	(let-values ([(action args tail) (irc-parse-rest tail)])
	  (if (or action args tail)
	      (irc-msg user action args tail)
	      #f))
	#f)))

(define (irc-parse-ping msg)
  (cond ((regexp-match re-ping msg) => second)
	(else #f)))


;; Output message validation and reformatting

;; maximum is 510 but the server prepends our user/hostname
(define irc-max-msg-length 480)

(define (valid-message-length? msg)
  (<= (string-length msg) irc-max-msg-length))

(define (irc-msg-line-count msg)
  (ceiling (/ (string-length msg) irc-max-msg-length)))

(define (irc-split-message msg)
  (let ([msg-length (length msg)]
	[line-length irc-max-msg-length])
    (define (split-msg start end)
      (cond [(>= start msg-length) '()]
	    [(cons (substring msg start (min msg-length end))
		   (split-msg end (+ end line-length)))]))
    (split-msg 0 line-length)))

(define (irc-clean-msg msg)
  (format "~a\r\n" (regexp-replace #px"[\r\n\t]" (~a msg) " ")))

;; Basic IRC commands

(define irc-out-thread (make-parameter #f))

(define (irc-out msg)
  (when (= (irc-msg-line-count msg) 1)
    (thread-send (irc-out-thread) (irc-clean-msg msg))))

(define (irc-declare-nick nick)
  (irc-out (format "NICK ~a" nick)))

(define (irc-declare-user user [realname user] [mode 0])
  (irc-out (format "USER ~a ~a * :~a" user mode realname)))

(define (irc-pong data)
  (irc-out (format "PONG :~a" data)))

(define (irc-privmsg chan msg)
  (irc-out (format "PRIVMSG ~a :~a" chan msg)))

(define (irc-whois nick)
  (irc-out (format "WHOIS ~a" nick)))

(define (irc-whowas nick)
  (irc-out (format "WHOWAS ~a" nick)))

(define (irc-join-channel chan)
  (irc-out (format "JOIN ~a" chan)))

(define (irc-part-channel chan)
  (irc-out (format "PART ~a" chan)))

(define (irc-quit [reason "Exiting"])
  (irc-out (format "QUIT :~a" reason)))
