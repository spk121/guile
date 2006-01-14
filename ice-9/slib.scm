;;;; slib.scm --- SLIB interface shim
;;;;
;;;;	Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004,
;;;;	2005 Free Software Foundation, Inc.
;;;;
;;;; This file is part of GUILE.
;;;; 
;;;; GUILE is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your
;;;; option) any later version.
;;;; 
;;;; GUILE is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with GUILE; see the file COPYING.  If not, write to the
;;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA
;;;;
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
;;;;

(if (and (defined? '*guile-use-old-ice-9-slib*) *guile-use-old-ice-9-slib*)
    (load-from-path "ice-9/slib-old.scm")
    (begin
      
      ;; The goal here is to eventually *only* rely on guile.init and
      ;; communicate any needed fixes upstream, but for now, we still
      ;; need to do a bit of the work here.
      ;;
      ;; At some point, we will probably just want to require a "new
      ;; enough" version of slib (perhaps with Guile 1.8).  In that
      ;; case, we'll probably just load guile.init, and then test the
      ;; version.

      ;; These can be removed if/when we require a "new enough"
      ;; version of SLIB.
      (define-module (ice-9 slib))
      (provide 'hash)

      (load-from-path "slib/guile.init")

      ;; This can be removed if/when we require a "new enough" version of SLIB.
      (if (not (defined? 'browse-url))
          ;; Nothing special to do for this, so straight from
          ;; Template.scm.  Maybe "sensible-browser" for a debian
          ;; system would be worth trying too (and would be good on a
          ;; tty).
          (define-public (browse-url url)
            (define (try cmd end) (zero? (system (string-append cmd url end))))
            (or (try "netscape-remote -remote 'openURL(" ")'")
                (try "netscape -remote 'openURL(" ")'")
                (try "netscape '" "'&")
                (try "netscape '" "'"))))))
