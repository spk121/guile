;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2023 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Backend-specific lowering and optimization when targetting Guile's
;;; bytecode virtual machine.
;;;
;;; Code:

(define-module (language cps guile-vm)
  #:use-module (ice-9 match)
  #:use-module (language cps guile-vm loop-instrumentation)
  #:use-module (language cps guile-vm lower-primcalls)
  #:use-module (language cps guile-vm reify-primitives)
  #:export (make-lowerer
            available-optimizations))

(define (make-lowerer optimization-level opts)
  (lambda (exp env)
    (add-loop-instrumentation
     (reify-primitives
      (lower-primcalls exp)))))

(define (available-optimizations)
  '())
