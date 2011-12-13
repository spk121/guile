(define-module (analyzer value-sets)
  #:use-module (srfi srfi-9)
  #:export (value-set value-set-type
                      make-value-set value-set?
                      value-set-values set-value-set-values!
                      value-set-properties set-value-set-properties!

            value-set-nothing value-set-anything
            value-set-can-be-anything? value-set-has-values?
            value-set-with-values
            value-set-value-satisfying))

#|

  To keep things simple in the beginning, it's best to just have a few simple types and a few compound types. We attempt the following four simple types:
  - booleans
  - numbers (but no differentiation within numbers)
  - strings
  - symbols
 and we use pairs as our only compound data type (this includes lists).

  We also need a small vocabulary of procedures. Here's one:
  - not
  - boolean?
  - number?
  - +
  - string?
  - symbol?
  - eq?
  
|#


(define-record-type value-set-type
  #| a value set has two sorts of things:
  - values is a list of individual Scheme values
  - properties is a list of representations of sets of Scheme values, like
  integers. the value-set describes the union of these two items

  properties is a list of possible descriptions. each description is a
  list of primitive predicates that this value would satisfy. so
  properties is like a propositional logic formula in disjunctive normal
  form.

  |#
  (value-set values properties)
  value-set?
  (values value-set-values set-value-set-values!)
  (properties value-set-properties set-value-set-properties!))

; convenience constructors
(define (value-set-anything)
  (value-set '() '((anything))))
(define (value-set-nothing)
  (value-set '() '()))
(define (value-set-with-values . vals)
  (value-set vals '()))

; and predicates
(define (value-set-has-values? vs)
  (or (not (null? (value-set-values vs)))
      (not (null? (value-set-properties vs)))))

(define (value-set-can-be-anything? vs)
  (let loop ((props (value-set-properties vs)))
    (cond ((null? props) #f)
          ((eq? (caar props) 'anything) #t)
          (else (loop (cdr props))))))

; and a selector
(define (value-set-value-satisfying vs pred)
  (let loop ((vals (value-set-values vs)))
    (cond ((null? vals) #f)
          ((pred (car vals)) (car vals))
          (else (loop (cdr vals))))))

(define-record-type primitive-procedure-type
  ;; this type holds the value-set version of a primitive procedure
  (primitive-procedure evaluator)
  primitive-procedure?
  (evaluator primitive-procedure-evaluator))

(define (vs-cons a b)
  (values
   (value-set (list (cons a b))
              '())
   '()))

(define (vs-car p)
  (if (value-set-can-be-anything? p)
      (value-set-anything)
      (let ((pair (value-set-value-satisfying p pair?)))
        (if pair
            (car pair)
            (value-set-nothing)))))

(define (vs-cdr p)
  (if (value-set-can-be-anything? p)
      (value-set-anything)
      (let ((pair (value-set-value-satisfying p pair?)))
        (if pair
            (car pair)
            (value-set-nothing)))))
