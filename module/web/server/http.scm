;;; Web I/O: HTTP

;; Copyright (C)  2010, 2011, 2012 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:
;;;
;;; This is the HTTP implementation of the (web server) interface.
;;;
;;; `read-request' sets the character encoding on the new port to
;;; latin-1.  See the note in request.scm regarding character sets,
;;; strings, and bytevectors for more information.
;;;
;;; Code:

(define-module (web server http)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (system repl error-handling)
  #:use-module (ice-9 poll)
  #:use-module (ice-9 async-queue)
  #:use-module (ice-9 thread-pool))


(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define (make-waker)
  (let ((wakeup (socketpair AF_UNIX SOCK_STREAM 0)))
    (fcntl (car wakeup) F_SETFL O_NONBLOCK)
    (fcntl (cdr wakeup) F_SETFL O_NONBLOCK)
    #;
    (if (defined? 'O_CLOEXEC)
        (begin
          ;; FIXME: currently it's not defined...
          (fcntl (car wakeup) F_SETFD O_CLOEXEC)
          (fcntl (cdr wakeup) F_SETFD O_CLOEXEC)))
    (values (lambda () (put-u8 (car wakeup) 0))
            (cdr wakeup))))

(define flush-wake-port
  ;; FIXME: need a bytevector-get-n! that understands nonblocking ports.
  (lambda (port)
    (get-u8 port)))

(define-record-type <http-server>
  (make-http-server socket poll-idx poll-set wake-thunk threaded?
                    read-workers read-queue handle-queue
                    write-workers write-queue keepalive-queue)
  http-server?
  (socket http-socket)
  (poll-idx http-poll-idx set-http-poll-idx!)
  (poll-set http-poll-set)
  (wake-thunk http-wake-thunk)
  (threaded? http-threaded?)

  (read-workers http-read-workers set-http-read-workers!)
  (read-queue http-read-queue)
  (handle-queue http-handle-queue)

  (write-workers http-write-workers set-http-write-workers!)
  (write-queue http-write-queue)
  (keepalive-queue http-keepalive-queue))

(define *error-events* (logior POLLHUP POLLERR))
(define *read-events* POLLIN)
(define *events* (logior *error-events* *read-events*))

;; -> server
(define* (http-open #:key
                    (host #f)
                    (family AF_INET)
                    (addr (if host
                              (inet-pton family host)
                              INADDR_LOOPBACK))
                    (port 8080)
                    (socket (make-default-socket family addr port))
                    (threaded? (and (provided? 'threads) #t))
                    (read-workers (if threaded? 8 1))
                    (write-workers (if threaded? 8 1)))
  (listen socket 128)
  (sigaction SIGPIPE SIG_IGN)
  (let ((poll-set (make-empty-poll-set)))
    (call-with-values make-waker
      (lambda (wake-thunk wake-port)
        (poll-set-add! poll-set socket *events*)
        (poll-set-add! poll-set wake-port *read-events*)
        (let ((read-queue (make-async-queue #:capacity read-workers))
              (handle-queue (make-async-queue #:capacity read-workers))
              (write-queue (make-async-queue #:capacity write-workers))
              (keepalive-queue (make-async-queue #:capacity write-workers
                                            #:fixed? #f)))
          (define server
            (make-http-server socket 0 poll-set wake-thunk threaded?
                              #f read-queue handle-queue
                              #f write-queue keepalive-queue))
          (if threaded?
              (begin
                (set-http-read-workers!
                 server
                 (make-thread-pool
                  read-workers
                  (lambda ()
                    (cond ((async-queue-pop! read-queue)
                           => (lambda (p) (read-request! server p)))))))
                (start-thread-pool! (http-read-workers server))
                (set-http-write-workers!
                 server
                 (make-thread-pool
                  write-workers
                  (lambda ()
                    (cond ((async-queue-pop! write-queue)
                           => (lambda (args)
                                (apply write-request! server args)))))))
                (start-thread-pool! (http-write-workers server))))
          server)))))

(define (bad-request port)
  (write-response (build-response #:version '(1 . 0) #:code 400
                                  #:headers '((content-length . 0)))
                  port))

(define (read-request! server port)
  (call-with-error-handling
   (lambda ()
     (cond
      ((eof-object? (peek-char port))
       ;; EOF.
       (close-port port))
      (else
       ;; Otherwise, try to read a request from this port.
       (with-throw-handler #t
         (lambda ()
           (let* ((req (read-request port))
                  (body (read-request-body req)))
             (or (async-queue-push! (http-handle-queue server)
                                    (list port req body))
                 (error "failed to push request during shutdown"))
             ((http-wake-thunk server))))
         (lambda (k . args)
           (define-syntax-rule (cleanup-catch statement)
             (catch #t
               (lambda () statement)
               (lambda (k . args)
                 (format (current-error-port) "In ~a:\n" 'statement)
                 (print-exception (current-error-port) #f k args))))
           (cleanup-catch (bad-request port))
           (cleanup-catch (close-port port)))))))
   #:pass-keys '(quit interrupt)
   #:on-error 'backtrace
   #:post-error
   (lambda (k . args)
     (display "While reading request:\n" (current-error-port))
     (print-exception (current-error-port) #f k args))))

(define (enqueue-read! server port)
  (if (http-threaded? server)
      (or (async-queue-push! (http-read-queue server) port)
          (false-if-exception
           (begin
             (warn "failed to push read during shutdown.")
             (close-port port))))
      (read-request! server port)))

;; -> (client request body | #f #f #f)
(define (http-read server)
  (let* ((poll-set (http-poll-set server)))
    (let lp ((idx (http-poll-idx server)))
      (let ((revents (poll-set-revents poll-set idx)))
        (cond
         ((async-queue-try-pop! (http-handle-queue server))
          => (lambda (vals) (apply values vals)))
         ((async-queue-try-pop! (http-keepalive-queue server))
          => (lambda (port)
               (poll-set-add! poll-set port *events*)
               (lp idx)))
         ((zero? idx)
          ;; The server socket, and the end of our downward loop.
          (cond
           ((zero? revents)
            ;; No client ready, and no error; poll and loop.
            (poll poll-set)
            (lp (1- (poll-set-nfds poll-set))))
           ((not (zero? (logand revents *error-events*)))
            ;; An error.
            (set-http-poll-idx! server idx)
            (throw 'interrupt))
           (else
            ;; A new client. Add to set, poll, and loop.
            ;;
            ;; FIXME: preserve meta-info.
            (let ((client (accept (poll-set-port poll-set idx))))
              ;; Buffer input and output on this port.
              (setvbuf (car client) _IOFBF)
              ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
              (setsockopt (car client) SOL_SOCKET SO_SNDBUF (* 12 1024))
              (poll-set-add! poll-set (car client) *events*)
              (poll poll-set)
              (lp (1- (poll-set-nfds poll-set)))))))
         ((zero? revents)
          ;; Nothing on this port.
          (lp (1- idx)))
         ((= idx 1)
          ;; The wakeup socket.
          (flush-wake-port (poll-set-port poll-set idx))
          (lp (1- idx)))
         ;; Otherwise, a client socket with some activity on
         ;; it. Remove it from the poll set.
         (else
          (let ((port (poll-set-remove! poll-set idx)))
            ;; Record the next index in all cases, in case enqueue-read!
            ;; throws an error.
            (set-http-poll-idx! server (1- idx))
            (enqueue-read! server port)
            (lp (1- idx)))))))))

(define (keep-alive? response)
  (let ((v (response-version response)))
    (and (or (< (response-code response) 400)
             (= (response-code response) 404))
         (case (car v)
           ((1)
            (case (cdr v)
              ((1) (not (memq 'close (response-connection response))))
              ((0) (memq 'keep-alive (response-connection response)))))
           (else #f)))))

(define (write-request! server client response body)
  (call-with-error-handling
   (lambda ()
     (let* ((response (write-response response client))
            (port (response-port response)))
       (cond
        ((not body))                    ; pass
        ((bytevector? body)
         (write-response-body response body))
        (else
         (error "Expected a bytevector for body" body)))
       (cond
        ((keep-alive? response)
         (force-output port)
         (or (async-queue-push! (http-keepalive-queue server) port)
             ;; Shutting down; there is a sane thing to do, no need to
             ;; error.
             (close-port port))
         ((http-wake-thunk server)))
        (else
         (close-port port)))))
   #:pass-keys '(quit interrupt)
   #:on-error 'backtrace
   #:post-error
   (lambda (k . args)
     (display "While writing response:\n" (current-error-port))
     (print-exception (current-error-port) #f k args))))

(define (enqueue-write! server client response body)
  (if (http-threaded? server)
      (or (async-queue-push! (http-write-queue server)
                             (list client response body))
          (false-if-exception
           (begin
             (warn "failed to push write during shutdown.")
             (close-port client))))
      (write-request! server client response body)))

;; -> 0 values
(define (http-write server client response body)
  (enqueue-write! server client response body)
  (values))

(define (seconds-from-now n)
  (let ((now (gettimeofday)))
    (cons (+ (car now) n) (cdr now))))

(define (async-queue-for-each queue proc)
  (let lp ()
    (cond ((async-queue-try-pop! queue)
           => (lambda (vals) (proc vals) (lp))))))

;; -> unspecified values
(define (http-close server)
  (let ((poll-set (http-poll-set server)))
    (display "Plugging read queue\n")
    (async-queue-stop-accepting! (http-read-queue server))
    (display "Stopping read workers\n")
    (if (http-threaded? server)
        (stop-thread-pool! (http-read-workers server)
                           (seconds-from-now 5)
                           #:cancel? #t))
    (display "Plugging read queue\n")
    (async-queue-stop-accepting! (http-read-queue server))
    (display "Draining read queue\n")
    (async-queue-for-each (http-read-queue server) close-port)
    (display "Plugging handle queue\n")
    (async-queue-stop-accepting! (http-handle-queue server))
    (display "Draining handle queue\n")
    (async-queue-for-each (http-handle-queue server)
                          (lambda (vals) (close-port (car vals))))
    (display "Plugging write queue\n")
    (async-queue-stop-accepting! (http-write-queue server))
    (display "Stopping write workers\n")
    (if (http-threaded? server)
        (stop-thread-pool! (http-write-workers server)
                           (seconds-from-now 5)
                           #:cancel? #t))
    (display "Draining write queue\n")
    (async-queue-for-each (http-write-queue server)
                          (lambda (vals) (close-port (car vals))))
    (display "Plugging keepalive queue\n")
    (async-queue-stop-accepting! (http-keepalive-queue server))
    (display "Draining keepalive queue\n")
    (async-queue-for-each (http-keepalive-queue server) close-port)
    (display "Closing poll ports\n")
    (let lp ((n (poll-set-nfds poll-set)))
      (if (positive? n)
          (begin
            (close-port (poll-set-remove! poll-set (1- n)))
            (lp (1- n)))))))

(define-server-impl http
  http-open
  http-read
  http-write
  http-close)
