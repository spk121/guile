;;; Dynamically linking foreign libraries via dlopen and dlsym
;;; Copyright (C) 2021 Free Software Foundation, Inc.
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
;;; Implementation of dynamic-link.
;;;
;;; Code:


(define-module (system foreign-library)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (guile-extensions-path
            ltdl-library-path
            guile-system-extensions-path

            lib->cyg
            load-foreign-library
            foreign-library?
            foreign-library-pointer
            foreign-library-function))

(define-record-type <foreign-library>
  (make-foreign-library filename handle)
  foreign-library?
  (filename foreign-library-filename)
  (handle foreign-library-handle set-foreign-library-handle!))

(eval-when (expand load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_system_foreign_library"))

(define system-library-extensions
  (cond
   ((string-contains %host-type "-darwin")
    '(".bundle" ".so" ".dylib"))
   ((or (string-contains %host-type "cygwin")
        (string-contains %host-type "mingw")
        (string-contains %host-type "msys"))
    '(".dll"))
   (else
    '(".so"))))

(define (has-extension? head exts)
  (match exts
    (() #f)
    ((ext . exts)
     (or (string-contains head ext)
         (has-extension? head exts)))))

(define (file-exists-with-extension head exts)
  (if (has-extension? head exts)
      (and (file-exists? head) head)
      (let lp ((exts exts))
        (match exts
          (() #f)
          ((ext . exts)
           (let ((head (string-append head ext)))
             (if (file-exists? head)
                 head
                 (lp exts))))))))

(define (file-exists-in-path-with-extension basename path exts)
  (match path
    (() #f)
    ((dir . path)
     (or (file-exists-with-extension (in-vicinity dir basename) exts)
         (file-exists-in-path-with-extension basename path exts)))))

(define path-separator
  (case (system-file-name-convention)
    ((posix) #\:)
    ((windows) #\;)
    (else (error "unreachable"))))

(define (parse-path var)
  (match (getenv var)
    (#f #f)
    ;; Ignore e.g. "export GUILE_SYSTEM_EXTENSIONS_PATH=".
    ("" '())
    (val (string-split val path-separator))))

;; Since 3.0.6, this is the preferred environment variable for
;; augmenting the search path for Guile extension libraries.  It has
;; highest priority.
(define guile-extensions-path
  (make-parameter
   (or (parse-path "GUILE_EXTENSIONS_PATH") '())))

;; After GUILE_EXTENSIONS_PATH, there is LTDL_LIBRARY_PATH. Before
;; 3.0.6, libtool was used. It allowed the paths in LTDL_LIBRARY_PATH
;; preference over using OS's dynamic linker's standard search strategy.
(define ltdl-library-path
  (make-parameter
   (or (parse-path "LTDL_LIBRARY_PATH") '())))

;; 3rd priority.
(define guile-system-extensions-path
  (make-parameter
   (or (parse-path "GUILE_SYSTEM_EXTENSIONS_PATH")
       (list (assq-ref %guile-build-info 'libdir)
             (assq-ref %guile-build-info 'extensiondir)))))

;; There are a few messy situations here related to libtool.
;;
;; Guile used to use libltdl, the dynamic library loader provided by
;; libtool.  This loader used LTDL_LIBRARY_PATH, and for backwards
;; compatibility we still support that path.
;;
;; However, libltdl would not only open ".so" (or ".dll", etc) files,
;; but also the ".la" files created by libtool.  In installed libraries
;; -- libraries that are in the target directories of "make install" --
;; .la files are never needed, to the extent that most GNU/Linux
;; distributions remove them entirely.  It is sufficient to just load
;; the ".so" (or ".dll", etc) files.
;;
;; But for uninstalled dynamic libraries, like those in a build tree, it
;; is a bit of a mess.  If you have a project that uses libtool to build
;; libraries -- which is the case for Guile, and for most projects using
;; autotools -- and you build foo.so in directory D, libtool will put
;; foo.la in D, but foo.so goes in D/.libs.
;;
;; The nice thing about ltdl was that it could load the .la file, even
;; from a build tree, preventing the existence of ".libs" from leaking
;; out to the user.
;;
;; We don't use libltdl now, essentially for flexibility and
;; error-reporting reasons.  But, it would be nice to keep this old
;; use-case working.  So as a stopgap solution, we add a ".libs" subdir
;; to the path for each entry in LTDL_LIBRARY_PATH, in case the .so is
;; there instead of alongside the .la file.

;; Furthermore, libtool --mode=execute augments the environment variable
;; LD_LIBRARY_PATH on Linux, where the shared library loader ld.so will
;; use it.  On Windows, however, libtool --mode=execute augments the
;; environment variable PATH.

(define (augment-ltdl-library-path path)
  "For each directory entry in the PATH list, inserts an additional
entry for the .libs subdirectory of that directory, when present."
  (match path
    (() '())
    ((dir . path)
     (cons* dir (in-vicinity dir ".libs")
            (augment-ltdl-library-path path)))))

(define (default-search-path search-ltdl-library-path?)
  "Returns the search paths for guile extensions. These are compile-time
defaults augmented by paths in the GUILE_EXTENSIONS_PATH and
LTDL_LIBRARY_PATH environment variables."
  (append
   (guile-extensions-path)
   (if search-ltdl-library-path?
       (augment-ltdl-library-path (ltdl-library-path))
       '())
   (guile-system-extensions-path)))

(define (lib->cyg name)
  "Convert a standard shared library name to a Cygwin shared library
name."
  (if (not name)
      #f
      (let ((start (1+ (or (string-index-right
                            name
                            (lambda (c) (or (char=? #\\ c) (char=? #\/ c))))
                           -1))))
        (cond
         ((>= (+ 3 start) (string-length name))
          name)
         ((string= name "lib" start (+ start 3))
          (string-append (substring name 0 start)
                         "cyg"
                         (substring name (+ start 3))))
         (else
          name)))))


(define *dll-search-dirs* '())
;; On Win32, loading a DLL will fail when the DLL depends on other DLLs
;; that are not in the dynamic link library search directories.  Should
;; one extension depend on another extension or a system DLL, paths in
;; GUILE_EXTENSIONS_PATH, LTDL_LIBRARY_PATH, and PATH need to be added
;; via add-dll-search-directory before trying to load a DLL.
(define base-path
  (make-parameter
   (or (parse-path "PATH") '())))

(define (add-dll-search-directories search-path)
  "This procedure adds a list of absolute paths to the set of directories
that are searched for a DLL."
  (define (addp path)
    (let ((cpath (false-if-exception (canonicalize-path path))))
      (when (and cpath
                 (eqv? 'directory (false-if-exception (stat:type (stat cpath))))
                 (not (member cpath *dll-search-dirs*)))
        (add-dll-search-directory cpath)
        (set! *dll-search-dirs* (cons cpath *dll-search-dirs*)))))
  (when (zero? (length *dll-search-dirs*))
    (for-each addp (base-path)))
  (for-each addp search-path)
  (pk 'dll-search *dll-search-dirs*))

(define* (load-foreign-library #:optional filename #:key
                               (extensions system-library-extensions)
                               (search-ltdl-library-path? #t)
                               (search-path (default-search-path
                                              search-ltdl-library-path?))
                               (search-system-paths? #t)
                               (lazy? #t) (global? #f) (rename-on-cygwin? #t))
  (define (error-not-found)
    (scm-error 'misc-error "load-foreign-library"
               "file: ~S, message: ~S"
               (list filename "file not found")
               #f))
  (define flags
    (logior (if lazy? RTLD_LAZY RTLD_NOW)
            (if global? RTLD_GLOBAL RTLD_LOCAL)))
  (define (dlopen* name) (dlopen name flags))
  (if (and rename-on-cygwin? (string-contains %host-type "cygwin"))
      (set! filename (lib->cyg filename)))

  (when (string-contains %host-type "mingw")
    (add-dll-search-directories search-path))

  (make-foreign-library
   filename
   (cond
    ((not filename)
     ;; The self-open trick.
     (dlopen* #f))
    ((or (absolute-file-name? filename)
         (string-any file-name-separator? filename))
     (cond
      ((or (file-exists-with-extension filename extensions)
           (and search-ltdl-library-path?
                (file-exists-with-extension
                 (in-vicinity (in-vicinity (dirname filename) ".libs")
                              (basename filename))
                 extensions)))
       => dlopen*)
      (else
       (error-not-found))))
    ((file-exists-in-path-with-extension filename search-path extensions)
     => dlopen*)
    (search-system-paths?
     (if (or (null? extensions) (has-extension? filename extensions))
         (dlopen* filename)
         (let lp ((extensions extensions))
           (match extensions
             ((extension)
              ;; Open in tail position to propagate any exception.
              (dlopen* (string-append filename extension)))
             ((extension . extensions)
              ;; If there is more than one extension, unfortunately we
              ;; only report the error for the last extension.  This is
              ;; not great because maybe the library was found with the
              ;; first extension, failed to load and had an interesting
              ;; error, but then we swallowed that interesting error and
              ;; proceeded, eventually throwing a "file not found"
              ;; exception.  FIXME to use more structured exceptions and
              ;; stop if the error that we get is more specific than
              ;; just "file not found".
              (or (false-if-exception
                   (dlopen* (string-append filename extension)))
                  (lp extensions)))))))
    (else
     (error-not-found)))))

(define (->foreign-library lib)
  (if (foreign-library? lib)
      lib
      (load-foreign-library lib)))

(define* (foreign-library-pointer lib name)
  (let ((handle (foreign-library-handle (->foreign-library lib))))
    (dlsym handle name)))

(define* (foreign-library-function lib name
                                   #:key
                                   (return-type void)
                                   (arg-types '())
                                   (return-errno? #f))
  (let ((pointer (foreign-library-pointer lib name)))
    (pointer->procedure return-type pointer arg-types
                        #:return-errno? return-errno?)))
