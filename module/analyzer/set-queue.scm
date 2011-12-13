(define-module (analyzer set-queue)
  #:export (make-set-queue
            set-queue-insert!
            set-queue-remove!
            set-queue-empty?
            emptying-set-queue!))

;; The following are helper functions for using list-based set-queues:
;; queues where an element is only inserted if it is not already in
;; the queue.
(define (make-set-queue)
  (cons '() '()))

(define (set-queue-insert! sq elt)
  (cond ((null? (car sq))
         (set-car! sq (list elt))
         (set-cdr! sq (car sq)))
        ((memq elt (car sq)))
        (else
         (let ((new-pair (list elt)))
           (set-cdr! (cdr sq) new-pair)
           (set-cdr! sq new-pair)))))

(define (set-queue-remove! sq)
  (let ((res (car sq)))
    (set-car! sq (cdr (car sq)))
    (if (eq? (cdr sq) res)
        (set-cdr! sq '()))
    (car res)))

(define (set-queue-empty? sq)
  (null? (car sq)))

(define (emptying-set-queue! sq func)
  (if (not (set-queue-empty? sq))
      (let ((next (set-queue-remove! sq)))
        (func next)
        (emptying-set-queue! sq func))))
