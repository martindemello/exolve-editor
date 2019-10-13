#lang racket/gui

(require "exolve.rkt")
(require "qxw.rkt")

(define courier-face
  (let ([delta (new style-delta%)])
    (send delta set-face "Courier")
    delta))

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
	     [label "Quit"]
	     [callback (λ (b e) (quit-app))])))

    (define text-panes
      (new horizontal-panel% [parent frame]))

    (define xword-canvas (new editor-canvas% [parent text-panes]))
    (define xword-text (new text%))
    (send xword-canvas set-editor xword-text)
    (send xword-text change-style courier-face)
    
    (define template-canvas (new editor-canvas% [parent text-panes]))
    (define template-text (new text%))
    (send template-canvas set-editor template-text)
    (send template-text change-style courier-face)

    (send frame show #t)

    (super-new)

    (define (load-qxw-file)
      (let* [(fname (get-file))
	     (text (exolve:format-xw (qxw:parse-file fname)))]
	(send xword-text delete)
	(send xword-text insert text)))
    
    (define (load-exolve-file)
      (let* [(fname (get-file))
	     (text (file->string fname))]
	(send template-text delete)
	(send template-text insert text)))

    (define (save-exolve-m-file)
      (let* [(fname (put-file))
	     (text (send xword-text get-text))
             (template (send template-text get-text))
             (out (exolve:merge template text))]
	(with-output-to-file fname
	  (thunk (display out)))))

    (define (copy-to-clipboard event)
      (send the-clipboard set-clipboard-string
	    (send xword-text get-text)
	    (send event get-time-stamp)))
    (define (quit-app) (exit))))

(define _ (new application%))
