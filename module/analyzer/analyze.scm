(define-module (analyzer analyze)
  #:use-module (analyzer value-sets)
  #:use-module (analyzer set-queue)
  #:use-module (analyzer lexical-envs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (language tree-il)
  #:use-module (system base syntax)
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

#|

The src slot is the same as for regular tree-il. The value-set slot
points to the value-set of this expression's return value.

|#
(define-type (<annotated-tree-il>
              #:common-slots (src parent can-return? return-value-set))
  ;; to do: add printer

  (<a-void>)
  (<a-const> exp)
  (<a-primitive-ref> name)
  (<a-lexical-ref> name gensym)
  (<a-lexical-set> target-value-set
                   name gensym exp)
  (<a-module-ref> mod name public?)
  (<a-module-set> target-value-set
                  mod name public? exp)
  (<a-toplevel-ref> name)
  (<a-toplevel-set> target-value-set
                    name exp)
  (<a-toplevel-define> name exp)
  (<a-conditional> test consequent alternate)
  (<a-call> proc args)
  (<a-seq> head tail)
  (<a-lambda> meta body)
  (<a-lambda-case> req opt rest kw inits gensyms body alternate)
  (<a-let> names gensyms vals body)
  (<a-letrec> in-order? names gensyms vals body)
  (<a-dynlet> fluids vals body)
  (<a-dynref> fluid)
  (<a-dynset> target-value-set fluid exp)
  (<a-dynwind> winder body unwinder)
  (<a-prompt> tag body handler)
  (<a-abort> tag args tail)
  (<a-fix> names gensyms vals body)
  (<a-let-values> exp body)
  (<a-verify> exps))

(define default-environment
  (environment-append-pairs (make-environment)
    (cons 'cons (value-set-with-values prim-cons))
    (cons 'car  (value-set-with-values prim-car))
    (cons 'cdr  (value-set-with-values prim-cdr))))

(define (primitive-lookup name)
  (environment-lookup default-environment name))

(define-syntax-rule (push! list obj)
  (set! list (cons obj list)))

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

;; this procedure
;; - converts tree-il to annotated tree-il.
;; - annotates nodes with their parents.
;; - annotates references and sets with the value-sets they use.
;;   (it creates value-set objects, but doesn't do inference)
;; - adds nodes to the *values-need-inference* set-queue
(define (tree-il->annotated-tree-il! tree-il)
  (let rec ((parent #f)
            (tree tree-il)
            (env default-environment))
    (match tree
           (($ <void> src)
            (let ((ret 
                   (make-a-void src parent
                                #t ; can-return?
                                (value-set-nothing) ; return-value-set
                                )))
              (child-gained-value! parent)
              ret))
           (($ <const> src exp)
            (let ((ret
                   (make-a-const src parent
                                 #t ; can-return?
                                 (value-set-with-values exp) ; return-value-set
                                 exp
                                 )))
              (child-gained-value! parent)
              ret))
           (($ <primitive-ref> src name)
            (let ((ret
                   (make-a-primitive-ref src parent
                                         #t ; can-return?
                                         (primitive-lookup name) ; return-value-set
                                         name)))
              (child-gained-value! parent)
              ret))
           (($ <lexical-ref> src name gensym)
            (make-a-lexical-ref src parent
                                #t ; can-return?
                                (annotated-tree-il-return-value-set
                                 (environment-lookup env gensym)) ; return-value-set
                                name gensym))
           (($ <lexical-set> src name gensym exp)
            (let ((ret (make-a-lexical-set src parent
                                           #t ; can-return?
                                           (value-set-nothing) ; return-value-set
                                           (environment-lookup env gensym) ; target-value-set
                                           name gensym
                                           #f)))
              (set! (a-lexical-set-exp) (rec ret exp env))
              ret))
           (($ <module-ref> src mod name public?)
            (error "No module-ref yet!"))
           (($ <module-set> src mod name public? exp)
            (error "No module-set yet!"))
           (($ <toplevel-ref> src name)
            (make-a-toplevel-ref src parent
                                 #t ; can-return?
                                 (environment-lookup env name) ; return-value-set
                                 name))
           (($ <toplevel-set> src name exp)
            (let ((ret (make-a-toplevel-set src parent
                                            #t ; can-return?
                                            (value-set-nothing) ; return-value-set
                                            (environment-lookup env name) ; target-value-set
                                            name
                                            #f)))
              (set! (a-toplevel-set-exp ret) (rec ret exp env))
              ret))
           (($ <toplevel-define> src name exp)
            (error "No top level defines yet!"))
           ;; don't need to put this in the *newly-set-value* list
           ;; because it will be put there once the leaves in its
           ;; definition have propagated a definition up to the top
           ;; level. until that happens we don't know enough to infer
           ;; anything interesting anyway.
           (($ <conditional> src test consequent alternate)
            (let ((ret (make-a-conditional src parent
                                           #t ; can-return?
                                           (value-set-nothing) ; return-value-set
                                           #f #f #f)))
              (set! (a-conditional-test ret) (rec ret test env))
              (set! (a-conditional-consequent ret) (rec ret consequent env))
              (set! (a-conditional-alternate ret) (rec ret alternate env))
              ret))
           (($ <call> src ($ <toplevel-ref> tsrc 'verify) args)
            (let ((ret (make-a-verify src parent
                                      #f ; can-return?
                                      (value-set-nothing) ; return-value-se
                                      '())))
              (set! (a-verify-exps ret)
                    (map (lambda (x) (rec ret x env)) args))
              (push! *verifies* ret)
              ret))
           (($ <call> src proc args)
            (let ((ret (make-a-call src parent
                                    #t ; can-return?
                                    (value-set-nothing) ; return-value-set
                                    #f '())))
              (set! (a-call-proc ret) (rec ret proc env))
              (set! (a-call-args ret) (map (lambda (x) (rec ret x env)) args))
              ret))
           (($ <primcall> src name args)
            (error "No primcalls!"))
            ;; To do: rewrite primcalls as (call (primitive-ref ...) ...)
           (($ <seq> src head tail)
            (let ((ret (make-a-seq src parent
                                   #t ; can-return?
                                   (value-set-nothing) ; return-value-set
                                   #f #f)))
              (set! (a-seq-head ret) (rec ret head env))
              (set! (a-seq-tail ret) (rec ret tail env))
              ret))
           (($ <lambda> src meta body)
            (let ((ret (make-a-lambda src parent
                                 #t ; can-return?
                                 (value-set-nothing) ; return-value-set
                                 meta '())))
              (set! (a-lambda-body ret) (rec ret body env))
              ret))
           (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
            (error "No lambda-case right now!"))
           (($ <let> src names gensyms vals body)
            (let ((ret (make-a-let src parent
                                   #t ; can-return?
                                   #f ; return-value-set
                                   names gensyms
                                   '() '())))
              (set! (a-let-vals ret) (map (lambda (x) (rec ret x env)) vals))
              (set! (a-let-body ret)
                    (rec ret body
                         (environment-append-names-values env
                                                          gensyms
                                                          (a-let-vals ret))))
              (set! (annotated-tree-il-return-value-set ret)
                    (annotated-tree-il-return-value-set (a-let-body ret)))
              ret))
           (($ <letrec> src in-order? names gensyms vals body)
            (let ((ret (make-a-letrec src parent
                                      #t ; can-return?
                                      (value-set-nothing) ; return-value-set
                                      in-order? names gensyms
                                      '() '())))
              (set! (a-letrec-vals ret) (map (lambda (x) (rec ret x env)) vals))
              (set! (a-letrec-body ret) (rec ret body env))
              ret))
           (($ <dynlet> src fluids vals body)
            (error "No dynlet yet!"))
           (($ <dynref> src fluid)
            (error "No dynref yet!"))
           (($ <dynset> src fluid exp)
            (error "No dynset yet!"))
           (($ <dynwind> src winder body unwinder)
            (error "No dynwind yet!"))
           (($ <prompt> src tag body handler)
            (error "No prompt yet!"))
           (($ <abort> src tag args tail)
            (error "No abort yet!"))
           (($ <let-values> src names gensyms exp body)
            (error "No let-values yet!"))
           (($ <fix> src names gensyms vals body)
            (error "No fix yet!"))
)))

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
  (set! *tree*
   (tree-il->annotated-tree-il!
    (compile sexp #:to 'tree-il)))
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
