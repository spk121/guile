;;; "Soft" ports
;;; Copyright (C) 2023 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Implementation of legacy soft-port interface.
;;;
;;; Code:


(define-module (ice-9 soft-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 custom-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs bytevectors gnu)
  #:export (make-soft-port))

(define (type-error proc expecting val)
  (scm-error 'wrong-type-arg proc "Wrong type (expecting `~S'): ~S"
             (list expecting val) (list val)))

(define (soft-port-read %get-char)
  (unless (procedure? %get-char)
    (type-error "soft-port-read" "procedure" %get-char))
  (define encode-buf-size 10)
  (define buffer (make-bytevector encode-buf-size))
  (define buffer-pos 0)
  (define buffer-len 0)
  (define transcoder
    (make-custom-binary-output-port
     "transcoder"
     (lambda (bv start count)
       (let ((to-copy (min encode-buf-size count)))
         (bytevector-copy! bv start buffer 0 to-copy)
         (set! buffer-pos 0)
         (set! buffer-len to-copy)
         to-copy))
     #f #f #f))
  (lambda (port bv start count)
    (let lp ((start start) (count count) (ret 0))
      (unless (< buffer-pos buffer-len)
        (match (%get-char)
          ((or #f (? eof-object?)) ret)
          (ch
           (unless (eq? (port-encoding port) (port-encoding transcoder))
             (set-port-encoding! transcoder (port-encoding port)))
           (unless (eq? (port-conversion-strategy port)
                        (port-conversion-strategy transcoder))
             (set-port-conversion-strategy! transcoder
                                            (port-conversion-strategy port)))
           (put-char transcoder ch)
           (force-output transcoder))))
      (let ((to-copy (min count (- buffer-len buffer-pos))))
        (bytevector-copy! buffer buffer-pos bv start to-copy)
        (set! buffer-pos (+ buffer-pos to-copy))
        to-copy))))

(define (soft-port-write %put-string %flush)
  (unless (procedure? %put-string)
    (type-error "soft-port-write" "procedure" %put-string))
  (when %flush
    (unless (procedure? %flush)
      (type-error "soft-port-write" "procedure" %flush)))
  (lambda (port bv start count)
    (let* ((bytes (bytevector-slice bv start count))
           (str (call-with-input-bytevector
                 bytes
                 (lambda (bport)
                   (set-port-encoding! bport (port-encoding port))
                   (set-port-conversion-strategy!
                    bport
                    (port-conversion-strategy port))
                   (get-string-all bport)))))
      (%put-string str)
      (if %flush (%flush))
      count)))

(define (soft-port-close %close)
  (unless (procedure? %close)
    (type-error "soft-port-close" "procedure" %close))
  (lambda (port) (%close)))

(define (soft-port-input-waiting? %input-ready)
  (unless (procedure? %input-ready)
    (type-error "soft-port-close" "procedure" %input-ready))
  (lambda (port) (< 0 (%input-ready))))

(define (%make-soft-port %put-char %put-string %flush %get-char %close
                         %input-ready reading? writing? buffering)
  (cond
   ((not (or reading? writing?))
    (%make-void-port ""))
   (else
    (let ((port
           (make-custom-port
            #:id "soft-port"
            #:read (and reading? (soft-port-read %get-char))
            #:write (and writing? (soft-port-write %put-string %flush))
            #:seek (lambda (port offset whence)
                     (error "soft ports are not seekable"))
            #:close (if %close
                        (soft-port-close %close)
                        (lambda (port) (values)))
            #:get-natural-buffer-sizes (lambda (port read-size write-size)
                                         ;; The in-practice expectation
                                         ;; is that soft ports have
                                         ;; unbuffered output.
                                         (values read-size 1))
            #:random-access? (lambda (port) #f)
            #:input-waiting? (if %input-ready
                                 (soft-port-input-waiting? %input-ready)
                                 (lambda (port) #t))
            #:close-on-gc? #t)))
      (when buffering
        (setvbuf port buffering))
      port))))

(define (make-soft-port vtable modes)
  "Return a port capable of receiving or delivering characters as
specified by the @var{modes} string (@pxref{File Ports, open-file}).
@var{pv} must be a vector of length 5 or 6.  Its components are as
follows:

@enumerate 0
@item
procedure accepting one character for output
@item
procedure accepting a string for output
@item
thunk for flushing output
@item
thunk for getting one character
@item
thunk for closing port (not by garbage collection)
@item
(if present and not @code{#f}) thunk for computing the number of
characters that can be read from the port without blocking.  @end
enumerate

For an output-only port only elements 0, 1, 2, and 4 need be procedures.
For an input-only port only elements 3 and 4 need be procedures.  Thunks
2 and 4 can instead be @code{#f} if there is no useful operation for
them to perform.

If thunk 3 returns @code{#f} or an @code{eof-object}
(@pxref{Input, eof-object?, ,r5rs, The Revised^5 Report on
Scheme}) it indicates that the port has reached end-of-file.
For example:

@lisp
(define stdout (current-output-port))
(define p (make-soft-port
           (vector
            (lambda (c) (write c stdout))
            (lambda (s) (display s stdout))
            (lambda () (display \".\" stdout))
            (lambda () (char-upcase (read-char)))
            (lambda () (display \"@@\" stdout)))
           \"rw\"))

(write p p) @result{} #<input-output: soft 8081e20>
@end lisp"
  (define reading?
    (or (string-index modes #\r)
        (string-index modes #\+)))
  (define writing?
    (or (string-index modes #\w)
        (string-index modes #\a)
        (string-index modes #\+)))
  (define buffering
    (and writing?
         (cond
          ((string-index modes #\0) 'none)
          ((string-index modes #\l) 'line)
          (else #f))))
  (match vtable
    (#(%put-char %put-string %flush %get-char %close)
     (%make-soft-port %put-char %put-string %flush %get-char %close #f
                      reading? writing? buffering))
    (#(%put-char %put-string %flush %get-char %close %chars-waiting)
     (%make-soft-port %put-char %put-string %flush %get-char %close
                      %chars-waiting
                      reading? writing? buffering))))
