(define-module (analyzer set-queue)
  #:use-module (srfi srfi-9)
  #:export (make-set-queue
            set-queue?
            set-queue-insert!
            set-queue-remove!
            set-queue-empty?
            emptying-set-queue!))

(define-record-type set-queue-type
  (%make-set-queue elts)
  set-queue?
  (elts sq-elts set-sq-elts!))

;; The following are helper functions for using list-based set-queues:
;; queues where an element is only inserted if it is not already in
;; the queue.
(define (make-set-queue)
  (%make-set-queue '()))

(define (set-queue-insert! sq elt)
  (if (null? (sq-elts sq))
      (set-sq-elts! sq (list elt))
      (let loop ((this (sq-elts sq)))
        (cond ((eq? (car this) elt))
              ((null? (cdr this)) (set-cdr! this (list elt)))
              (else (loop (cdr this)))))))

(define (set-queue-remove! sq)
  (let ((old-list (sq-elts sq)))
    (set-sq-elts! sq (cdr old-list))
    (car old-list)))

(define (set-queue-empty? sq)
  (null? (sq-elts sq)))

(define (emptying-set-queue! sq func)
  (if (not (set-queue-empty? sq))
      (let ((next (set-queue-remove! sq)))
        (func next)
        (emptying-set-queue! sq func))))
