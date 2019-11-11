#lang racket/base

(require compiler/distribute
	 compiler/embed
	 file/zip
	 pkg/lib
	 racket/file)

(define app-exe "exolve-editor.exe")
(define dist-dir "ExolveEditor")
(define zip-file (string-append dist-dir ".zip"))
(define pkg-deps '("functional"))

(define (install-deps)
  (for ([dep pkg-deps])
    (if (pkg-directory dep)
      (displayln (format "Already installed: ~a" dep))
      (with-pkg-lock
	(pkg-install (list (pkg-desc dep 'name #f #f #f)))))))

(define (make-exe)
  (parameterize ([use-compiled-file-paths (list "compiled")])
    (create-embedding-executable
      app-exe
      #:modules '((#f "exolve-editor.rkt"))
      #:gracket? #t)))

(define (make-dist)
  (and (directory-exists? dist-dir)
    (delete-directory/files dist-dir))
  (and (file-exists? zip-file)
    (delete-file zip-file))
  (assemble-distribution
    dist-dir
    (list app-exe)))

(define (zip-dist)
  (zip zip-file dist-dir))

(module+ main
  (displayln "Installing dependencies")
  (install-deps)
  (displayln "Compiling")
  (make-exe)
  (displayln "Packaging")
  (make-dist)
  (zip-dist)
  (displayln (format "Created package: ~a" zip-file)))
