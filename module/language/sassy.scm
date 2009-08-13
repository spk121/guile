;;; Sassy

;; Copyright (C) 2009 Free Software Foundation, Inc.
;; Copyright (C) 2005 Jonathan Kraut

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; Contact:
;; Jonathan Kraut
;; 4130 43 ST #C2
;; Sunnyside, NY 11104
;; jak76@columbia.edu

;;; Code:

(define-module (language sassy)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevector)
  #:use-module (rnrs io ports)

  #:export (sassy
            sassy-data-align
            sassy-data-list
            sassy-data-size
            sassy-data-stack
            sassy-entry-point
            sassy-expand
            sassy-heap-align
            sassy-heap-size
            sassy-hexdump
            sassy-make-bin
            sassy-make-elf
            sassy-print-relocs
            sassy-print-symbols
            sassy-reloc-list
            sassy-reloc-name
            sassy-reloc-offset
            sassy-reloc-patcher
            sassy-reloc-section
            sassy-reloc-type
            sassy-reloc-value
            sassy-reloc-width
            sassy-symbol-exists?
            sassy-symbol-name
            sassy-symbol-offset
            sassy-symbol-scope
            sassy-symbol-section
            sassy-symbol-size
            sassy-symbol-table
            sassy-symbol-unres
            sassy-text-align
            sassy-text-list
            sassy-text-org
            sassy-text-size
            sassy-text-stack))


(define (write-byte b . port)
  (put-u8 (if (null? port) (current-output-port) (car port))
          b))
(define (read-byte . port)
  (get-u8 (if (null? port) (current-input-port) (car port))))

(define (hash-table-ref t k . th)
  (cond ((hash-ref t k))
	(else (if (null? t) #f ((car th))))))

(define hash-table-set! hash-set!)

(define (alist->hash-table lst)
  (let ((t (make-hash-table)))
    (for-each (lambda (itm)
		(hash-table-set! t (car itm) (cdr itm)))
	      lst)
    t))

(define (hash-table-values t)
  (hash-map->list (lambda (k v) v) t))

;; HACK: we know we're compiling from a certain dir, so encode like
;; this. Nasty.
(include "language/sassy/extras.scm")
(include "language/sassy/meta-lambda.scm")
(include "language/sassy/push-stacks.scm")
(include "language/sassy/api.scm")
(include "language/sassy/intern.scm")
(include "language/sassy/macros.scm")
(include "language/sassy/numbers.scm")

;; The original sassy included other/srfi-56-pieces, but we can use
;; bytevectors for that.
(define (float32->byte-list float)
  (let ((bv (make-bytevector 4)))
    (bytevector-ieee-single-native-set! bv 0 float)
    (bytevector->u8-list bv)))
(define (float64->byte-list float)
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-native-set! bv 0 float)
    (bytevector->u8-list bv)))

(include "language/sassy/operands.scm")
(include "language/sassy/text-block.scm")
(include "language/sassy/opcodes.scm")
(include "language/sassy/text.scm")
(include "language/sassy/parse.scm")
(include "language/sassy/main.scm")

(include "language/sassy/flat-bin.scm")
(include "language/sassy/elf.scm")

; (load "tests/run-tests.scm")
; (sassy-run-tests 'all)
