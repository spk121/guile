(define-module (analyzer lexical-envs)
  #:export (make-environment
            environment-append-pairs
            environment-append-names-values
            environment-lookup))

;; we will represent environments as association lists.

(define (make-environment)
  '())

;; append some name-value pairs to an environment
;; environments match names to value-sets.
(define (environment-append-pairs env . args)
  (let inner ((arg-lst args))
    (if (null? arg-lst)
        env
        (cons (car arg-lst)
              (inner (cdr arg-lst))))))

;; the difference between environment-append-pairs and
;; environment-append-names-values is that in the first one, you have
;; pairs of (name, value), and in the second, you have a list of names
;; and a matching list of values

(define (environment-append-names-values env names values)
  (cond ((and (null? names) (null? values))
         env)
        ((or (null? names) (null? values))
         (error "environment-append-names-values got different-length lists!"))
        (else
         (cons (cons (car names) (car values))
               (environment-append-names-values env (cdr names) (cdr values))))))

(define (environment-lookup env name)
  (assq-ref env name))
