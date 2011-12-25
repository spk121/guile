(define-module (analyzer annotated-tree-il)
  #:use-module (analyzer value-sets)
  #:use-module (analyzer set-queue)
  #:use-module (analyzer lexical-envs)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:use-module (language tree-il)
  #:export (annotated-tree-il-src
            annotated-tree-il-parent
            annotated-tree-il-can-return?
            annotated-tree-il-return-value-set

            <a-void> a-void? make-a-void
            
            <a-const> a-const? make-a-const a-const-exp
            
            <a-primitive-ref> a-primitive-ref? a-primitive-ref-name
            
            <a-lexical-ref> a-lexical-ref? a-lexical-ref-name
            a-lexical-ref-gensym

            <a-lexical-set> a-lexical-set? a-lexical-set-target-value-set
            a-lexical-set-name a-lexical-set-gensym a-lexical-set-exp

            <a-module-ref> a-module-ref? a-module-ref-mod a-module-ref-name
            a-module-ref-public?

            <a-module-set> a-module-set? a-module-set-target-value-set
            a-module-set-mod a-module-set-name a-module-set-public?
            a-module-set-exp

            <a-toplevel-ref> a-toplevel-ref? a-toplevel-ref-name

            <a-toplevel-set> a-toplevel-set? a-toplevel-set-target-value-set
            a-toplevel-set-name a-toplevel-set-exp

            <a-toplevel-define> a-toplevel-define? a-toplevel-define-name
            a-toplevel-define-exp

            <a-conditional> a-conditional? a-conditional-test
            a-conditional-consequent a-conditional-alternate

            <a-call> a-call? a-call-proc a-call-args

            <a-seq> a-seq? a-seq-head a-seq-tail

            <a-lambda> a-lambda? a-lambda-meta a-lambda-body

            <a-lambda-case> a-lambda-case? a-lambda-case-req a-lambda-case-opt a-lambda-case-rest
            a-lambda-case-kw a-lambda-case-inits a-lambda-case-gensyms a-lambda-case-body
            a-lambda-case-alternate

            <a-let> a-let? a-let-names a-let-gensyms a-let-vals a-let-body

            <a-letrec> a-letrec? a-letrec-in-order? a-letrec-names
            a-letrec-gensyms a-letrec-vals a-letrec-body

            <a-dynlet> a-dynlet? a-dynlet-fluids a-dynlet-vals a-dynlet-body

            <a-dynref> a-dynref? a-dynref-fluid

            <a-dynset> a-dynset? a-dynset-target-value-set a-dynset-fluid
            a-dynset-exp

            <a-dynwind> a-dynwind? a-dynwind-winter a-dynwind-body
            a-dynwind-handler

            <a-prompt> a-prompt? a-prompt-tag a-prompt-body a-prompt-handler

            <a-abort> a-abort? a-abort-tag a-abort-args a-abort-tail

            <a-fix> a-fix? a-fix-names a-fix-gensyms a-fix-vals a-fix-body

            <a-let-values> a-let-values? a-let-values-exp a-let-values-body

            <a-verify> a-verify? a-verify-exps

            tree-il->annotated-tree-il!))

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

;; this procedure
;; - converts tree-il to annotated tree-il.
;; - annotates nodes with their parents.
;; - annotates references and sets with the value-sets they use.
;;   (it creates value-set objects, but doesn't do inference)
;; - adds verify nodes to verifies, a variable object holding a list
;; - calls leaf-func on nodes that already have values (const nodes),
;;   after annotated with parents and value sets
(define (tree-il->annotated-tree-il! tree-il toplevel-env verifies leaf-func)
  (let rec ((parent #f)
            (tree tree-il)
            (env toplevel-env))
    (match tree
           (($ <void> src)
            (error "No voids yet!"))
           (($ <const> src exp)
            (let ((ret
                   (make-a-const src parent
                                 #t ; can-return?
                                 (value-set-with-values exp) ; return-value-set
                                 exp
                                 )))
              (leaf-func ret)
              ret))
           (($ <primitive-ref> src name)
            (error "No primitive-refs yet!"))
           (($ <lexical-ref> src name gensym)
            (make-a-lexical-ref src parent
                                #t ; can-return?
                                (annotated-tree-il-return-value-set
                                 (environment-lookup env gensym)) ; return-value-set
                                name gensym))
           (($ <lexical-set> src name gensym exp)
            (error "No lexical sets yet!"))
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
            (error "No toplevel sets yet!"))
           (($ <toplevel-define> src name exp)
            (error "No top level defines yet!"))
           ;; don't need to put this in the *newly-set-value* list
           ;; because it will be put there once the leaves in its
           ;; definition have propagated a definition up to the top
           ;; level. until that happens we don't know enough to infer
           ;; anything interesting anyway.
           (($ <conditional> src test consequent alternate)
            (error "No conditionals yet!"))
           (($ <call> src ($ <toplevel-ref> tsrc 'verify) args)
            (let ((ret (make-a-verify src parent
                                      #f ; can-return?
                                      (value-set-nothing) ; return-value-se
                                      '())))
              (set! (a-verify-exps ret)
                    (map (lambda (x) (rec ret x env)) args))
              (variable-set! verifies
                             (cons ret (variable-ref verifies)))
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
            (error "No seqs yet!"))
           (($ <lambda> src meta body)
            (error "No lambdas yet!"))
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
            (error "No letrecs yet!"))
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
