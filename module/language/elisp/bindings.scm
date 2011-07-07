;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language elisp bindings)
  #:use-module (srfi srfi-9)
  #:export (make-bindings
            mark-global!
            map-globals
            with-lexical-bindings
            with-dynamic-bindings
            get-lexical-binding))

;;; This module defines routines to handle analysis of symbol bindings
;;; used during elisp compilation.  This data allows to collect the
;;; symbols, for which globals need to be created, or mark certain
;;; symbols as lexically bound.
;;;
;;; Needed globals are stored in an association-list that stores a list
;;; of symbols for each module they are needed in.
;;;
;;; The lexical bindings of symbols are stored in a hash-table that
;;; associates symbols to fluids; those fluids are used in the
;;; with-lexical-binding and with-dynamic-binding routines to associate
;;; symbols to different bindings over a dynamic extent.

;;; Record type used to hold the data necessary.

(define-record-type bindings
  (%make-bindings globals lexical-bindings)
  bindings?
  (globals globals set-globals!)
  (lexical-bindings lexical-bindings set-lexical-bindings!))

;;; Construct an 'empty' instance of the bindings data structure to be
;;; used at the start of a fresh compilation.

(define (make-bindings)
  (%make-bindings '() (make-hash-table)))

;;; Mark that a given symbol is needed as global in the specified
;;; slot-module.

(define (mark-global! bindings sym module)
  (let* ((old-globals (globals bindings))
         (old-in-module (or (assoc-ref old-globals module) '()))
         (new-in-module (if (memq sym old-in-module)
                            old-in-module
                            (cons sym old-in-module)))
         (new-globals (assoc-set! old-globals module new-in-module)))
    (set-globals! bindings new-globals)))

;;; Cycle through all globals needed in order to generate the code for
;;; their creation or some other analysis.

(define (map-globals bindings proc)
  (let iterate-modules ((mod-tail (globals bindings))
                        (mod-result '()))
    (if (null? mod-tail)
        mod-result
        (iterate-modules
         (cdr mod-tail)
         (let* ((aentry (car mod-tail))
                (module (car aentry))
                (symbols (cdr aentry)))
           (let iterate-symbols ((sym-tail symbols)
                                 (sym-result mod-result))
             (if (null? sym-tail)
                 sym-result
                 (iterate-symbols (cdr sym-tail)
                                  (cons (proc module (car sym-tail))
                                        sym-result)))))))))

;;; Get the current lexical binding (gensym it should refer to in the
;;; current scope) for a symbol or #f if it is dynamically bound.

(define (get-lexical-binding bindings sym)
  (let* ((lex (lexical-bindings bindings))
         (slot (hash-ref lex sym #f)))
    (if slot
        (fluid-ref slot)
        #f)))

;;; Establish a binding or mark a symbol as dynamically bound for the
;;; extent of calling proc.

(define (with-symbol-bindings bindings syms targets proc)
  (if (or (not (list? syms))
          (not (and-map symbol? syms)))
      (error "can't bind non-symbols" syms))
  (let ((lex (lexical-bindings bindings)))
    (for-each (lambda (sym)
                (if (not (hash-ref lex sym))
                    (hash-set! lex sym (make-fluid))))
              syms)
    (with-fluids* (map (lambda (sym) (hash-ref lex sym)) syms)
                  targets
                  proc)))

(define (with-lexical-bindings bindings syms targets proc)
  (if (or (not (list? targets))
          (not (and-map symbol? targets)))
      (error "invalid targets for lexical binding" targets)
      (with-symbol-bindings bindings syms targets proc)))

(define (with-dynamic-bindings bindings syms proc)
  (with-symbol-bindings bindings
                        syms
                        (map (lambda (el) #f) syms)
                        proc))
