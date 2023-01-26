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

;; This file defines a Guix package.  It can be used to spawn an
;; interactive development environment:
;;
;;   guix shell
;;
;; Or it can be used to build Guile from a checkout in an isolated
;; environment:
;;
;;   guix build -f guix.scm
;;
;; Likewise, you may cross-compile it:
;;
;;   guix build -f guix.scm --target=x86_64-w64-mingw32
;;
;; … or perform a native build for another architecture, assuming
;; either offloading or transparent QEMU emulation is set up:
;;
;;   guix build -f guix.scm -s riscv64-linux

(use-modules (guix)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages pkg-config))

(let ((vcs-file? (or (git-predicate (current-source-directory))
                     (const #t)))
      (S specification->package))
  (package
    (name "guile")
    (version "3.0.000-git")
    (source (local-file "." "guile-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~'("--enable-mini-gmp"
               #$@(if (target-x86-32?) ;<https://issues.guix.gnu.org/49368>
                      '("--disable-static" "CFLAGS=-g -O2 -fexcess-precision=standard")
                      '("--disable-static")))     ;saves 3 MiB

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'set-version
                 (lambda _
                   ;; Tell 'git-version-gen' what version this is, or it will
                   ;; just pick "UNKNOWN", making it unusable as a replacement
                   ;; for 'guile-3.0'.  XXX: This is inaccurate when using
                   ;; '--with-branch' but using (package-version this-package)
                   ;; wouldn't give us a valid version string.
                   (call-with-output-file ".tarball-version"
                     (lambda (port)
                       (display #$version port)))

                   ;; Set this one as well so 'version.test' passes.
                   (substitute* "GUILE-VERSION"
                     (("^GUILE_MICRO_VERSION=.*")
                      "GUILE_MICRO_VERSION=99\n"))))
               (add-before 'configure 'pre-configure
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Tell (ice-9 popen) the file name of Bash.
                   (let ((bash (false-if-exception
                                (search-input-file inputs "/bin/sh"))))
                     (substitute* "module/ice-9/popen.scm"
                       ;; If bash is #f allow fallback for user to provide
                       ;; "bash" in PATH.  This happens when cross-building to
                       ;; MinGW for which we do not have Bash yet.
                       (("/bin/sh")
                        (or bash "/bin/sh")))))))))

    (native-inputs
     (append (map S '("autoconf"
                      "automake"
                      "libtool"
                      "gettext"
                      "flex"
                      "texinfo"
                      "texlive-base"              ;for "make pdf"
                      "texlive-epsf"
                      "gperf"
                      "git"
                      "gdb"
                      "strace"
                      "readline"
                      "lzip"))

             ;; Ensure we get a cross-pkg-config when needed.
             (list pkg-config)

             ;; When cross-compiling, a native version of Guile itself
             ;; is needed.
             (if (%current-target-system)
                 (list this-package)
                 '())))
    (inputs
     (append (list (S "libffi"))

             ;; We need Bash when cross-compiling because some of the
             ;; scripts in bin/ refer to it.  Use 'bash-minimal' because
             ;; we don't need an interactive Bash with Readline and all.
             (if (target-mingw?)
                 (list (S "libiconv"))
                 (list (S "bash-minimal")))))
    (propagated-inputs
     (map S '("libunistring" "libgc")))

    (outputs '("out" "debug"))

    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache")))))
    (synopsis "Scheme implementation intended especially for extensions")
    (description
     "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
    (home-page "https://www.gnu.org/software/guile/")
    (license license:lgpl3+)))
