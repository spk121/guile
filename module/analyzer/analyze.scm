(define-module (analyzer analyze)
  #:use-module (analyzer value-sets)
  #:use-module (analyzer set-queue)
  #:use-module (analyzer lexical-envs)
  #:use-module (analyzer annotated-tree-il)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base compile)

  #:export (go))

#|

Our goal is to turn a collection of tree-il forms into a collection
of "tree-il instances", represented with the <annotated-tree-il> data
type.

The annotated-tree-il contains the program structure (just like the
Tree-IL) and also whatever information we have been able to infer
about the program at that location.

The current inference plan uses only one annotated-tree-il node for
every tree-il node. We create these nodes with the function
tree-il->annotated-tree-il and then walk the program, adding elements
to value-sets, stopping when we have nothing else to add, and hoping
to add very few elements.

In the future, we might like to switch to a representation with more
than one annotated-tree-il node per tree-il node, so we can represent
situations where the same function is called with very different
arguments.

|#

(define default-environment
  (environment-append-pairs (make-environment)
    (cons 'cons (value-set-with-values prim-cons))
    (cons 'car  (value-set-with-values prim-car))
    (cons 'cdr  (value-set-with-values prim-cdr))))

(define (primitive-lookup name)
  (environment-lookup default-environment name))

(define *values-need-inference* (make-set-queue))

(define *verifies* '())

;; this procedure is called on a node whose child node gained a
;; value. it decides what to do about this. the parent can be #f, which
;; means the child is at the top level
(define (child-gained-value! parent)
  (match parent
         (#f #t)
         (($ <a-call> _ _ _ _ _ _)
          (set-queue-insert! *values-need-inference* parent))
         (else #t)))

(define (all-verifies-pass?)
  (let outer ((v *verifies*))
    (if (null? v)
        #t
        (let inner ((exps (a-verify-exps (car v))))
          (cond ((null? exps) (outer (cdr v)))
                ((and (value-set-has-values?
                       (annotated-tree-il-return-value-set (car exps)))
                      (not (value-set-has-value?
                            (annotated-tree-il-return-value-set (car exps))
                            #f)))
                 (inner (cdr exps)))
                (else #f))))))

(define *tree* '())

;; This function starts with the annotated tree-il nodes in
;; *values-need-inference* and infers as many value sets as it can, with
;; the following limitations:
;;  - each tree node can only have one value set, and they do not
;;  support logical implication
;;  - it assumes that each function will return to its default
;;  continuation (for now).
;; It uses *values-need-inference* as a queue.
(define (infer-value-sets!)
  (emptying-set-queue! *values-need-inference*
    (lambda (node)
      (match node
        (($ <a-call> src parent can-return? return-value-set
                     proc args)
         (if (and (value-set-has-values?
                   (annotated-tree-il-return-value-set proc))
                  (value-set-has-no-properties?
                   (annotated-tree-il-return-value-set proc))
                  (every (lambda (x) (value-set-has-values?
                                 (annotated-tree-il-return-value-set x)))
                         args))
             (let loop ((procs (value-set-values
                                (annotated-tree-il-return-value-set proc))))
               (if (not (null? procs))
                   (begin
                     (let ((eval (primitive-procedure-evaluator (car procs))))
                       (apply eval return-value-set
                              (map annotated-tree-il-return-value-set
                                   args)))
                     (loop (cdr procs)))))
             )))
      )))

(define (go sexp)
  (set! *values-need-inference* (make-set-queue))
  (set! *verifies* '())
  (let ((verifies-box (make-variable '())))
    (set! *tree*
          (tree-il->annotated-tree-il!
           (compile sexp #:to 'tree-il)
           default-environment
           verifies-box
           (lambda (leaf) (child-gained-value!
                      (annotated-tree-il-parent leaf)))))
    (set! *verifies* (variable-ref verifies-box)))
  (infer-value-sets!)
  (all-verifies-pass?))

#|

This procedure attempts to annotate each expression in the tree-il
with the most specific value set that can describe that expression at
any point in this program. This is basically Hindley-Milner type
inference, except that it will catch enumerations. Note for myself
that there are two other choices of annotation style:

- You might compute the most general type that a given procedure could
possibly accept. This is what you would do if you wanted to make a
compiled library version of a procedure with no specialization. But I
think that whole idea is wrong.

- You might also compute the most specific types of every expression
at all points in the program, allowing duplication. This would let you
compile to the fastest possible code, and is what I hope to do later.

|#
