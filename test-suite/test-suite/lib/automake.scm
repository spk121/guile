;;;; test-suite/lib/automake.scm --- support for automake driven tests
;;;; Copyright (C) 2023 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.LESSER.
;;;; If not, write to the Free Software Foundation, Inc., 51 Franklin
;;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (test-suite lib automake)
  :use-module ((ice-9 match))
  :use-module ((srfi srfi-1) :select (drop-right last))
  :export (reporter))

(define (display->str x)
  (call-with-output-string (lambda (port) (display x port))))

(define (write->str x)
  (call-with-output-string (lambda (port) (write x port))))

(define (show port . args)
  (for-each (lambda (x) (display x port)) args))

(define (render-name name)
  (string-join (append (map display->str (drop-right name 1))
                       ;; Because for some tests, say via pass-if or
                       ;; pass-if-equal with no explict name, it's an
                       ;; arbirary form, possibly including null chars,
                       ;; etc.
                       (list (write->str (last name))))
               ": "))

(define (reporter trs-port)
  (match-lambda*
    (('pass name) (show trs-port ":test-result: PASS " (render-name name) "\n"))
    (('upass name) (show trs-port ":test-result: XPASS " (render-name name) "\n"))
    (('fail name) (show trs-port ":test-result: FAIL " (render-name name) "\n"))
    (('xfail name . args) (show trs-port ":test-result: XFAIL " (render-name name) "\n"))
    (('untested name) (show trs-port ":test-result: SKIP " (render-name name) "\n"))
    (('unsupported name) (show trs-port ":test-result: SKIP " (render-name name) "\n"))
    (('unresolved name) (show trs-port ":test-result: SKIP " (render-name name) "\n"))
    (('error name . args)
     (show trs-port ":test-result: ERROR " (render-name name) " ")
     (write args trs-port)
     (newline trs-port))))
