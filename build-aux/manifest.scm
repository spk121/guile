;;; Copyright © 2023 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Guile.
;;;
;;; GNU Guile is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guile is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guile.  If not, see <http://www.gnu.org/licenses/>.

;; This file defines a Guix manifest for use by Cuirass, the continuous
;; integration service running at <https://ci.guix.gnu.org>.

(use-modules (guix)
             (guix profiles))

(define guile
  (load "../guix.scm"))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "'")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define native-builds
  (manifest
   (map (lambda (system)
          (package->manifest-entry* guile system))
        '("x86_64-linux" "i686-linux"
          "aarch64-linux" "armhf-linux"
          "powerpc64le-linux"))))

(define cross-builds
  (manifest
   (map (lambda (target)
          (package->manifest-entry* guile "x86_64-linux"
                                    #:target target))
        '("i586-pc-gnu"
          ;; "arm-linux-gnueabihf"
          "aarch64-linux-gnu"
          "riscv64-linux-gnu"
          "i686-w64-mingw32"
          "x86_64-linux-gnu"))))

(concatenate-manifests (list native-builds cross-builds))
