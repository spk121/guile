;;; Thread pools

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
;;; A simple thread pool implementation.
;;;
;;; Code:

(define-module (ice-9 thread-pool)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 threads)
  #:export (make-thread-pool
            start-thread-pool!
            pause-thread-pool!
            stop-thread-pool!))


(define-record-type <worker>
  (%make-worker mutex condvar thread handler state)
  worker?
  (mutex worker-mutex)
  (condvar worker-condvar)
  (thread worker-thread set-worker-thread!)
  (handler worker-handler)
  (state worker-state set-worker-state!))

(define (make-worker handler)
  (%make-worker (make-mutex) (make-condition-variable) #f handler 'stopped))

(define* (worker-wait worker #:optional time)
  (if time
      (wait-condition-variable (worker-condvar worker)
                               (worker-mutex worker)
                               time)
      (wait-condition-variable (worker-condvar worker)
                               (worker-mutex worker))))

(define (worker-stopping? worker)
  (with-mutex (worker-mutex worker)
    (let lp ()
      (case (worker-state worker)
        ((running)
         #f)
        ((pausing)
         (set-worker-state! worker 'paused)
         (signal-condition-variable (worker-condvar worker))
         (lp))
        ((paused)
         (worker-wait worker)
         (lp))
        ((stopping)
         #t)
        (else
         (error "bad state" worker))))))

(define (worker-loop worker)
  (let lp ()
    (or (worker-stopping? worker)
        (begin
          ((worker-handler worker))
          (lp)))))

(define (pause-worker worker)
  (with-mutex (worker-mutex worker)
    (let lp ()
      (case (worker-state worker)
        ((running)
         (set-worker-state! worker 'pausing)
         (lp))
        ((pausing)
         #f)
        ((paused)
         #t)
        ((stopping)
         (error "attempt to go stopping -> pausing" worker))
        ((stopped)
         (set-worker-state! worker 'pausing)
         (set-worker-thread! worker
                             (make-thread worker-loop worker))
         (lp))
        (else
         (error "bad state" worker))))))

(define* (wait-for-paused worker #:optional time)
  (with-mutex (worker-mutex worker)
    (let lp ()
      (case (worker-state worker)
        ((paused)
         #t)
        (else
         (and (worker-wait worker time)
              (lp)))))))

(define (start-worker worker)
  (with-mutex (worker-mutex worker)
    (let lp ()
      (case (worker-state worker)
        ((running)
         #t)
        ((stopping)
         (error "attempt to go stopping -> running" worker))
        (else
         (set-worker-state! worker 'running)
         (if (worker-thread worker)
             (signal-condition-variable (worker-condvar worker))
             (set-worker-thread! worker (make-thread worker-loop worker)))
         (lp))))))

(define (stop-worker worker)
  (with-mutex (worker-mutex worker)
    (let lp ()
      (case (worker-state worker)
        ((stopped)
         #t)
        ((stopping)
         #f)
        (else
         (set-worker-state! worker 'stopping)
         (lp))))))

(define* (wait-for-stopped worker #:optional time #:key cancel?)
  (let ((thread
         (with-mutex (worker-mutex worker)
           (cond
            ((eq? (worker-state worker) 'stopped)
             #f)
            ((thread-exited? (worker-thread worker))
             (set-worker-thread! worker #f)
             (set-worker-state! worker 'stopped)
             #f)
            (else
             (worker-thread worker))))))
    (or (not thread)
        (begin
          (if time
              (join-thread (worker-thread worker) time)
              (join-thread (worker-thread worker)))
          (cond
           ((thread-exited? thread)
            (wait-for-stopped worker))
           (cancel?
            (cancel-thread (worker-thread worker))
            ;; Assume it works.
            (join-thread (worker-thread worker))
            (wait-for-stopped worker))
           (else
            #f))))))

(define-record-type <thread-pool>
  (%make-thread-pool size workers)
  thread-pool?
  (size thread-pool-size)
  (workers thread-pool-workers))

;; Create a thread pool, and bring it to the "paused" state.
;;
(define (make-thread-pool size handler)
  (let ((pool (%make-thread-pool size
                                 (map (lambda (i) (make-worker handler))
                                      (iota size)))))
    (pause-thread-pool! pool)
    pool))

(define* (pause-thread-pool! pool #:optional time)
  (for-each pause-worker (thread-pool-workers pool))
  (for-each (lambda (w) (wait-for-paused w time)) (thread-pool-workers pool)))

(define (start-thread-pool! pool)
  (for-each start-worker (thread-pool-workers pool)))

(define* (stop-thread-pool! pool #:optional time #:key cancel?)
  (for-each stop-worker (thread-pool-workers pool))
  (for-each (lambda (w) (wait-for-stopped w time #:cancel? cancel?))
            (thread-pool-workers pool)))
