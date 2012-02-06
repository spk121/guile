;;; Asynchronous queues

;; Copyright (C)  2012 Free Software Foundation, Inc.

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
;;; An implementation of thread-safe asynchronous queues, with both
;;; blocking and nonblocking interfaces.
;;;
;;; Code:

(define-module (ice-9 async-queue)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:export (make-async-queue
            async-queue?
            async-queue-length async-queue-capacity
            async-queue-stop-accepting! async-queue-restart! 
            async-queue-push!
            async-queue-pop! async-queue-try-pop!))

;; One thing that we should be careful about is to avoid exposing
;; information about the way this queue is implemented.
;;
;; Currently we use an array, but it's easy to imagine a functional
;; implementation facilitated by compare-and-swap operations, with
;; perhaps the option to disable the blocking interfaces (and thereby
;; remove the need for the mutex and cond var).
;;

(define-record-type <async-queue>
  (make-aq mutex condvar buf capacity length read-idx fixed? underflow-thunk)
  async-queue?
  (mutex aq-mutex)
  (condvar aq-condvar)
  (buf aq-buf set-aq-buf!)
  (capacity aq-capacity set-aq-capacity!)
  (length aq-length set-aq-length!)
  (read-idx aq-read-idx set-aq-read-idx!)
  (fixed? aq-fixed?)
  (underflow-thunk aq-underflow-thunk set-aq-underflow-thunk!))

(set-record-type-printer!
 <async-queue>
 (lambda (aq port)
   (format port "<async-queue ~x ~a/~a>" (object-address aq)
           (aq-length aq) (aq-capacity aq))))

(define (aq-inc! aq)
  (set-aq-length! aq (1+ (aq-length aq)))
  (signal-condition-variable (aq-condvar aq)))

(define (aq-dec! aq)
  (set-aq-length! aq (1- (aq-length aq)))
  (signal-condition-variable (aq-condvar aq)))

(define (aq-idx aq idx)
  (modulo idx (aq-capacity aq)))

(define (aq-wait aq time)
  (if time
      (wait-condition-variable (aq-condvar aq) (aq-mutex aq) time)
      (wait-condition-variable (aq-condvar aq) (aq-mutex aq))))

(define* (make-async-queue #:key (capacity 10) (fixed? #t))
  (make-aq (make-mutex)
           (make-condition-variable)
           (make-vector capacity #f)
           capacity
           0
           0
           fixed?
           #f))

(define (async-queue-length aq)
  (with-mutex (aq-mutex aq)
    (aq-length aq)))

(define (async-queue-capacity aq)
  (with-mutex (aq-mutex aq)
    (aq-capacity aq)))

(define* (async-queue-stop-accepting! aq #:optional
                                      (underflow-thunk (lambda () #f)))
  (with-mutex (aq-mutex aq)
    (set-aq-underflow-thunk! aq underflow-thunk)
    (broadcast-condition-variable (aq-condvar aq))))

(define (async-queue-restart! aq)
  (with-mutex (aq-mutex aq)
    (set-aq-underflow-thunk! aq #f)
    (broadcast-condition-variable (aq-condvar aq))))

(define* (async-queue-push! aq item #:optional time)
  (with-mutex (aq-mutex aq)
    (let lp ()
      (cond
       ((aq-underflow-thunk aq)
        ;; Not accepting values any more.
        #f)
       ((< (aq-length aq) (aq-capacity aq))
        (let ((idx (aq-idx aq (+ (aq-read-idx aq) (aq-length aq)))))
          (vector-set! (aq-buf aq) idx item)
          (aq-inc! aq)
          #t))
       ((aq-fixed? aq)
        (and (aq-wait aq time)
             (lp)))
       (else
        (let* ((capacity (* (aq-capacity 2)))
               (buf (make-vector capacity #f)))
          (vector-move-left! (aq-buf aq) (aq-read-idx aq) (aq-capacity aq)
                             buf 0)
          (vector-move-left! (aq-buf aq) 0 (aq-read-idx aq)
                             buf (aq-read-idx aq))
          (set-aq-buf! aq buf)
          (set-aq-capacity! aq capacity)
          (set-aq-read-idx! aq 0)
          (lp)))))))

(define* (async-queue-pop! aq #:optional time
                           #:key (default (lambda () #f)))
  ((with-mutex (aq-mutex aq)
     (let lp ()
       (if (zero? (aq-length aq))
           (or (aq-underflow-thunk aq)
               (if (aq-wait aq time)
                   (lp)
                   default))
           (let* ((idx (aq-read-idx aq))
                  (item (vector-ref (aq-buf aq) idx)))
             (vector-set! (aq-buf aq) idx #f)
             (set-aq-read-idx! aq (aq-idx aq (1+ idx)))
             (aq-dec! aq)
             (lambda () item)))))))

(define* (async-queue-try-pop! aq
                               #:key (default (lambda () #f)))
  ((with-mutex (aq-mutex aq)
     (if (zero? (aq-length aq))
         default
         (let* ((idx (aq-read-idx aq))
                (item (vector-ref (aq-buf aq) idx)))
           (vector-set! (aq-buf aq) idx #f)
           (set-aq-read-idx! aq (aq-idx aq (1+ idx)))
           (aq-dec! aq)
           (lambda () item))))))
