#lang racket/gui

(require racket/path)
(require browser/htmltext)

(require data/monad)
(require data/either)


(require "exolve.rkt")
(require "functional.rkt")
(require "qxw.rkt")
(require "reddit.rkt")

(provide application%)

(define courier-face
  (let ([delta (new style-delta%)])
    (send delta set-face "Courier")
    delta))

(define help-text%
  (html-text-mixin text%))

(define help-dialog%
  (class object%
    (init-field parent)
    (define help-canvas (new editor-canvas% [parent parent]))
    (define help-text (new help-text% [auto-wrap #t]))
    (send help-canvas set-editor help-text)
    (call-with-input-file "help.html" (λ (in) (render-html-to-text in help-text #f #f)))
    (send help-text lock #t)
    (super-new)))

(define (make-editor)
  (let [(editor (new text%))]
    (send editor change-style courier-face)
    editor))

(define (replace-text editor text)
  (send editor select-all)
  (send editor clear)
  (send editor insert text))

(define (save-text-to-file text)
  (let* [(fname (put-file))]
    (and fname
         (with-output-to-file fname #:exists 'replace
           (thunk (display text))))
    fname))

(define copyable-editor%
  (class object%
    (init-field parent)
    (init-field editor)
    (init-field application)
    (super-new)
    (define panel (new vertical-panel% [parent parent]
                       [style '(border)]))
    (define toolbar (new horizontal-panel% [parent panel]
                         [stretchable-height #f]
                         [alignment '(right center)]))
    (define canvas (new editor-canvas% [parent panel]))
    (send canvas set-editor editor)

    (define (set-status . args)
      (send/apply application set-status args))

    (define/public (set-editor e)
      (set! editor e)
      (send canvas set-editor e))

    (define copy-button
      (new button% [parent toolbar]
           [label "Copy to clipboard"]
           [callback (λ (b e) (copy-to-clipboard e))]))

    (define save-button
      (new button% [parent toolbar]
           [label "Save as"]
           [callback (λ (b e) (save-to-file))]))

    (define (copy-to-clipboard event)
      (send the-clipboard set-clipboard-string
            (send editor get-text)
            (send event get-time-stamp)))

    (define (save-to-file)
      (let* [(text (send editor get-text))
             (fname (save-text-to-file text))]
        (if fname
            (set-status "Saved file " (some-system-path->string fname))
            (set-status "File not saved"))))))

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
            [label "Load Exolve"]
            [callback (λ (b e) (load-exolve-grid))])
       (new button% [parent toolbar1]
            [label "Import QXW"]
            [callback (λ (b e) (load-qxw-file))])
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
            [label "Output Reddit"]
            [callback (λ (b e) (output-reddit))])))

    (define text-panes
      (new tab-panel% [parent frame]
           [choices (list "Crossword" "Template" "Output")]
           [min-height 600]
           [callback
            (λ (tp event) (set-selected-tab tp))]))

    (define (set-selected-tab tp)
      (let [(editor
             (case (send tp get-item-label (send tp get-selection))
               [("Crossword") xword]
               [("Template") template]
               [("Output") output]))]
        (send editor-view set-editor editor)))

    (define (set-tab n)
      (send text-panes set-selection n)
      (set-selected-tab text-panes))

    (define xword (make-editor))
    (define template (make-editor))
    (define output (make-editor))

    (define editor-view (new copyable-editor%
                             [parent text-panes]
                             [editor xword]
                             [application this]))

    (define statusbar
      (new horizontal-panel% [parent frame] [stretchable-height #f]))
    (define status (new message% [parent statusbar] [label "Welcome to exolve editor"]))
    (define spacer (new grow-box-spacer-pane% [parent statusbar]))

    (send frame show #t)

    (super-new)

    (define/public (set-status msg . rst)
      (send status set-label (string-join (append (list msg) rst) "")))

    (define (error-string e)
      (match e
        ['format-error "Error formatting ~a"]
        ['parse-error "Error parsing ~a"]
        ['read-error "Error reading ~a"]
        ['no-filename #f]))

    (define (load-and-process-file textbox parse fmt)
      (define f #f)
      (let* [(text
              (do [fname <- (try (get-file) 'no-filename)]
                (define _ (set! f (some-system-path->string fname)))
                [contents <- (try (file->string fname) 'read-error)]
                [xw <- (try (parse contents) 'parse-error)]
                (try (fmt xw) 'format-error)))
             (status
              (either error-string
                      (λ (t) (begin (replace-text textbox t)
                                    "Loaded file ~a"))
                      text))]
        (and status (set-status (format status f)))))

    (define (load-qxw-file)
      (load-and-process-file xword qxw:parse exolve:format-xw))

    (define (load-exolve-grid)
      (load-and-process-file xword exolve:extract-xw identity))

    (define (load-exolve-file)
      (load-and-process-file template identity identity))

    (define (save-exolve-m-file)
      (let* [(text (send xword get-text))
             (xw (exolve:parse-xw text))
             (template (send template get-text))
             (out (exolve:merge template xw))
             (fname (save-text-to-file out))]
        (if fname
            (set-status "Saved file " (some-system-path->string fname))
            (set-status "File not saved"))))

    (define (output-reddit)
      (let* [(text (send xword get-text))
             (xw (exolve:parse-xw text))
             (out (reddit:format-xw xw))]
        (replace-text output out)
        (set-tab 2)))

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
