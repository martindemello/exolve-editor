#lang racket/gui

(require racket/path)

(require "exolve.rkt")
(require "qxw.rkt")

(provide application%)

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

(define help-dialog%
  (class object%
    (init-field parent)
    (define help-canvas (new editor-canvas% [parent parent]))
    (define help-text (new text% [auto-wrap #t]))
    (send help-canvas set-editor help-text)
    (send help-text insert *help*)
    (send help-text lock #t)
    (super-new)))


(define copyable-editor%
  (class object%
    (init-field parent)
    (super-new)
    (define panel (new vertical-panel% [parent parent]
                       [style '(border)]))
    (define toolbar (new horizontal-panel% [parent panel]
                         [stretchable-height #f]
                         [alignment '(right center)]))
    (define canvas (new editor-canvas% [parent panel]))
    (define editor (new text%))
    (send canvas set-editor editor)
    (send editor change-style courier-face)

    (define copy-button
      (new button% [parent toolbar]
           [label "Copy to clipboard"]
           [callback (λ (b e) (copy-to-clipboard e))]))

    (define (copy-to-clipboard event)
      (send the-clipboard set-clipboard-string
            (send editor get-text)
            (send event get-time-stamp)))

    (define/public (get-text)
      (send editor get-text))

    (define/public (replace-text text)
      (send editor select-all)
      (send editor clear)
      (send editor insert text))))

(define application%
  (class object%
    (define frame (new frame% [label "Exolve Editor"]))

    (define toolbar1
      (new horizontal-panel% [parent frame] [stretchable-height #f]))

    (define toolbar2
      (new horizontal-panel% [parent frame] [stretchable-height #f]))

    (define buttons1
      (list
       (new button% [parent toolbar1]
            [label "Load QXW"]
            [callback (λ (b e) (load-qxw-file))])
       (new button% [parent toolbar1]
            [label "Load Exolve"]
            [callback (λ (b e) (load-exolve-grid))])
       (new button% [parent toolbar1]
            [label "Load Exolve template"]
            [callback (λ (b e) (load-exolve-file))])
       (new button% [parent toolbar1]
            [label "Help"]
            [callback (λ (b e) (help-dialog))])
       (new button% [parent toolbar1]
            [label "Quit"]
            [callback (λ (b e) (quit-app))])))

    (define buttons2
      (list
       (new button% [parent toolbar2]
            [label "Save Exolve"]
            [callback (λ (b e) (save-exolve-m-file))])
       (new button% [parent toolbar2]
            [label "Save Reddit"]
            [callback (λ (b e) (save-exolve-m-file))])))

    (define text-panes
      (new horizontal-panel% [parent frame]
           [min-height 600]))

    (define xword (new copyable-editor% [parent text-panes]))
    (define template (new copyable-editor% [parent text-panes]))

    (define statusbar
      (new horizontal-panel% [parent frame] [stretchable-height #f]))
    (define status (new message% [parent statusbar] [label "Welcome to exolve editor"]))
    (define spacer (new grow-box-spacer-pane% [parent statusbar]))

    (send frame show #t)

    (super-new)

    (define (set-status msg . rst)
      (send status set-label (string-join (append (list msg) rst) "")))

    (define (load-and-parse-file textbox parse format)
      (let [(fname (get-file))]
        (if fname
            (let* [(f (some-system-path->string fname))
                   (contents (file->string fname))
                   (xw (parse contents))]
              (if xw
                  (let [(text (format xw))]
                    (if text
                        (begin
                          (send textbox replace-text text)
                          (set-status "Loaded file " f))
                        (set-status "Error reading file " f)))
                  (set-status "Error reading file " f)))
            (set-status "File not loaded"))))

    (define (load-qxw-file)
      (load-and-parse-file xword qxw:parse exolve:format-xw))

    (define (load-exolve-grid)
      (load-and-parse-file xword exolve:extract identity))

    (define (load-exolve-file)
      (let [(fname (get-file))]
        (if fname
            (let [(text (file->string fname))]
              (send template replace-text text)
              (set-status "Loaded file " (some-system-path->string fname)))
            (set-status "File not loaded"))))

    (define (save-exolve-m-file)
      (let* [(fname (put-file))
             (text (send xword get-text))
             (template (send template get-text))
             (out (exolve:merge template text))]
        (if fname
            (begin
              (with-output-to-file fname #:exists 'replace
                (thunk (display out)))
              (set-status "Saved file " (some-system-path->string fname)))
            (set-status "File not saved"))))

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