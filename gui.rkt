#lang racket/gui

(require racket/path)

(require "exolve.rkt")
(require "qxw.rkt")

(define *help*
  (string-join
   (list
    "Click 'Load QXW' to load a qxw file. The file will be converted to exolve."
    "After that, you can 'Copy to clipboard' to copy the generated exolve text,"
    "and paste it into your html file, or 'Load exolve template' followed by "
    "'Save exolve' to merge your crossword into an exolve html file."
    "\n\nThe exolve template is the exolve.html or exolve-m.html file that"
    "ships with exolve; download it from https://github.com/viresh-ratnakar/exolve")
   " "))

(define courier-face
  (let ([delta (new style-delta%)])
    (send delta set-face "Courier")
    delta))

(define (replace-text editor text)
  (send editor select-all)
  (send editor clear)
  (send editor insert text))

(define help-dialog%
  (class object%
    (init-field parent)
    (define help-canvas (new editor-canvas% [parent parent]))
    (define help-text (new text% [auto-wrap #t]))
    (send help-canvas set-editor help-text)
    (send help-text insert *help*)
    (send help-text lock #t)
    (super-new)))

(define application%
  (class object%
    (define frame (new frame% [label "Exolve Editor"]))

    (define toolbar
      (new horizontal-panel% [parent frame] [stretchable-height #f]))

    ; Make a button in the frame
    (define buttons
      (list
       (new button% [parent toolbar]
            [label "Load QXW"]
            [callback (λ (b e) (load-qxw-file))])
       (new button% [parent toolbar]
            [label "Copy to clipboard"]
            [callback (λ (b e) (copy-to-clipboard e))])
       (new button% [parent toolbar]
            [label "Load Exolve template"]
            [callback (λ (b e) (load-exolve-file))])
       (new button% [parent toolbar]
            [label "Save Exolve"]
            [callback (λ (b e) (save-exolve-m-file))])
       (new button% [parent toolbar]
            [label "Help"]
            [callback (λ (b e) (help-dialog))])
       (new button% [parent toolbar]
            [label "Quit"]
            [callback (λ (b e) (quit-app))])))

    (define text-panes
      (new horizontal-panel% [parent frame]
           [min-height 600]))

    (define xword-canvas (new editor-canvas% [parent text-panes]))
    (define xword-text (new text%))
    (send xword-canvas set-editor xword-text)
    (send xword-text change-style courier-face)

    (define template-canvas (new editor-canvas% [parent text-panes]))
    (define template-text (new text%))
    (send template-canvas set-editor template-text)
    (send template-text change-style courier-face)

    (define statusbar
      (new horizontal-panel% [parent frame] [stretchable-height #f]))
    (define status (new message% [parent statusbar] [label "Welcome to exolve editor"]))
    (define spacer (new grow-box-spacer-pane% [parent statusbar]))

    (send frame show #t)

    (super-new)

    (define (set-status msg . rst)
      (send status set-label (string-join (append (list msg) rst) "")))

    (define (load-qxw-file)
      (let [(fname (get-file))]
        (if fname
            (let [(f (some-system-path->string fname))
                  (xw (qxw:parse-file fname))]
              (if xw
                  (let [(text (exolve:format-xw xw))]
                    (if text
                        (begin
                          (replace-text xword-text text)
                          (set-status "Loaded file " f))
                        (set-status "Error reading file " f)))
                  (set-status "Error reading file " f)))
            (set-status "File not loaded"))))

    (define (load-exolve-file)
      (let [(fname (get-file))]
        (if fname
            (let [(text (file->string fname))]
              (replace-text template-text text)
              (set-status "Loaded file " (some-system-path->string fname)))
            (set-status "File not loaded"))))

    (define (save-exolve-m-file)
      (let* [(fname (put-file))
             (text (send xword-text get-text))
             (template (send template-text get-text))
             (out (exolve:merge template text))]
        (if fname
            (begin
              (with-output-to-file fname #:exists 'replace
                (thunk (display out)))
              (set-status "Saved file " (some-system-path->string fname)))
            (set-status "File not saved"))))

    (define (copy-to-clipboard event)
      (send the-clipboard set-clipboard-string
            (send xword-text get-text)
            (send event get-time-stamp)))

    (define (help-dialog)
      (define d (new dialog% [parent frame]
                     [label "Help"]
                     [width 800]
                     [height 600]
                     [stretchable-width #t]
                     [stretchable-height #t]))
      (define h (new help-dialog% [parent d]))
      (send d show #t))

    (define (quit-app) (exit))))

(define _ (new application%))
