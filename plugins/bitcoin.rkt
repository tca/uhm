#lang racket

(require json
         (prefix-in srfi/48: srfi/48)
         net/url
         racket/date
         "../irclib.rkt")

(provide irc-bitcoin)
 
(define bars "▁▂▃▄▅▆▇█")
(define bar-count (string-length bars))
 
(define (sparks ns)
  (let* ([mn (apply min ns)]
	 [bar-width (/ (- (apply max ns) mn) (- bar-count 1))])
    (apply string (for/list ([n ns]) (string-ref bars (exact-floor (/ (- n mn) bar-width)))))))
 
(define (timestamp) (date->seconds (current-date)))

(define (cache-fn interval fn)
  (let ([cache (box #f)]
        [timer (box #f)])
    (lambda args
      (let ([now (timestamp)]
            [then (unbox timer)])
        (when (or (not then) (and then (>= (- now then) interval)))
          (set-box! timer now)
          (set-box! cache (apply fn args)))
        (unbox cache)))))

;; https://blockchain.info/charts/market-price?timespan=30days
(define historical-uri "http://blockchain.info/charts/market-price?showDataPoints=false&timespan=30days&daysAverageString=7&format=json")

(define get-historical
  (cache-fn
   (* 60 60)
   (lambda ()
     (call/input-url (string->url historical-uri) get-pure-port port->string))))

(define (run)
  (let* ([data (string->jsexpr (get-historical))]
         [data1 (hash-ref data 'values)]
         [prices (map (lambda (x) (hash-ref x 'y)) data1)]
         [last (hash-ref (last data1) 'y)])
    (srfi/48:format "last: ~0,2F 30day: ~a"  last (sparks prices))))

(define (handle-msg msg)
  (match msg
    [(irc-msg (irc-user nick _ _) "PRIVMSG" chan (pregexp "^;bit" (list _)))
     (irc-privmsg chan (format "~a: ~a" nick (run)))]
    [_ '()]))

(define (loop)
  (handle-msg (thread-receive))
  (loop))

(define (irc-bitcoin)
  (thread loop))
