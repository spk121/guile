(define-module (analyzer value-sets)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (value-set value-set-type
                      make-value-set value-set?
                      value-set-values set-value-set-values!
                      value-set-properties set-value-set-properties!

            value-set-nothing value-set-anything
            value-set-can-be-anything? value-set-has-values?
            value-set-has-value? value-set-has-property?
            value-set-nothing? value-set-has-no-properties?
            value-set-with-values
            value-set-value-satisfying
            
            value-set-union!
            value-set-add-value!
            value-set-add-property!

            vs-cons
            vs-car
            vs-cdr

            primitive-procedure-type
            primitive-procedure
            primitive-procedure?
            primitive-procedure-evaluator

            prim-cons prim-car prim-cdr))

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

;; convenience constructors
(define (value-set-anything)
  (value-set '() '((anything))))
(define (value-set-nothing)
  (value-set '() '()))
(define (value-set-with-values . vals)
  (value-set vals '()))

;; and predicates
(define (value-set-has-values? vs)
  (or (not (null? (value-set-values vs)))
      (not (null? (value-set-properties vs)))))

(define (value-set-can-be-anything? vs)
  (let loop ((props (value-set-properties vs)))
    (cond ((null? props) #f)
          ((eq? (caar props) 'anything) #t)
          (else (loop (cdr props))))))

(define (value-set-nothing? vs)
  (and (null? (value-set-values vs))
       (null? (value-set-properties vs))))

(define (value-set-has-value? vs v)
  (memq v (value-set-values vs)))

(define (value-set-has-property? vs p)
  (assq p (value-set-properties vs)))

(define (value-set-has-no-properties? vs)
  (null? (value-set-properties vs)))

;; and a selector
(define (value-set-value-satisfying vs pred)
  (let loop ((vals (value-set-values vs)))
    (cond ((null? vals) #f)
          ((pred (car vals)) (car vals))
          (else (loop (cdr vals))))))

;; and three modifiers. these are really three cases of the same thing -
;; a general case and two special ones. they are the basic operation on
;; value sets.

;; this function sets t to the union of t and x.
;; it uses a recursive merge if one of the values is a pair.
(define (value-set-union! t x)
  (cond ((value-set-can-be-anything? x)
         (set-value-set-values! t '())
         (set-value-set-properties! t '((anything))))
        (else
         (for-each (lambda (v) (value-set-add-value! t v))
                   (value-set-values x))
         (for-each (lambda (p) (value-set-add-property! t p))
                   (value-set-properties x)))))

(define (value-set-add-value! t v)
  (if (pair? v)
      (let ((old-pair (value-set-value-satisfying t pair?)))
        (if old-pair
            (begin (value-set-union! (car old-pair) (car v))
                   (value-set-union! (cdr old-pair) (cdr v)))
            (set-value-set-values! t (cons v (value-set-values t)))))
      (if (not (memv v (value-set-values t)))
          (set-value-set-values! t (cons v (value-set-values t))))))

(define (value-set-add-property! t p)
  (cond ((equal? p '(anything))
         (set-value-set-properties! t '((anything)))
         (set-value-set-values! t '()))
        ((equal? p '(number?))
         (set-value-set-properties! t '((number?))))
        (else
         (error "Don't know how to add property" p))))

(define-record-type primitive-procedure-type
  ;; this type holds the value-set version of a primitive procedure
  (primitive-procedure evaluator)
  primitive-procedure?
  (evaluator primitive-procedure-evaluator))

;; all procedures take an extra first argument, the "target", which is
;; the value set of their return value.
(define (vs-cons t a b)
  (value-set-add-value! t
                        (cons a b)))

(define (vs-car t p)
  (if (value-set-can-be-anything? p)
      (value-set-union! t (value-set-anything))
      (let ((pair (value-set-value-satisfying p pair?)))
        (if pair
            (value-set-union! t (car pair))))))

(define (vs-cdr t p)
  (if (value-set-can-be-anything? p)
      (value-set-union! t (value-set-anything))
      (let ((pair (value-set-value-satisfying p pair?)))
        (if pair
            (value-set-union! t (cdr pair))))))

(define prim-cons (primitive-procedure vs-cons))
(define prim-car  (primitive-procedure vs-car))
(define prim-cdr  (primitive-procedure vs-cdr))
