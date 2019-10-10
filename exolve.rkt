#lang racket

(require "xword.rkt")
(require "qxw.rkt")

(define (indent n str)
  (string-append (build-string n (λ (_) #\space)) str))

(define (row->str row)
  (string-join (vector->list row) ""))

(define (grid->lines grid)
  (vector->list (vector-map row->str grid)))

(define (headers xw)
  (let* [(rows (number->string (xword-rows xw)))
         (cols (number->string (xword-cols xw)))
         (headers (list
                   '("id" "unique-id")
                   '("title" "Title")
                   '("setter" "Setter")
                   `("width" ,cols)
                   `("height" ,rows)
                   `("copyright" "year Name")))]
    (map (λ (k-v) (string-append "exolve-" (first k-v) ": " (second k-v)))
         headers)))

(define (print-line ind line)
  (displayln (indent ind line)))

(define (print-lines ind lines)
  (for [(line lines)] (print-line ind line)))

(define (print ind line-or-lines)
  (cond [(list? line-or-lines) (print-lines ind line-or-lines)]
        [else (print-line ind line-or-lines)]))

(define (format-light cells n)
  (let [(word (string-join cells ""))
        (num (number->string n))
        (enum (number->string (length cells)))]
    (string-append num ". " word " (" enum ") Annotation")))

(define (print-xw xw)
  (let [(ac '())
        (dn '())
        (fmt (λ (fn i j n) (format-light (fn xw i j) n)))]
    (renumber xw
              #:on-ac (λ (i j n) (set! ac (cons (fmt word-ac+ i j n) ac)))
              #:on-dn (λ (i j n) (set! dn (cons (fmt word-dn+ i j n) dn))))   
    (print 0 "exolve-begin")
    (print 2 (headers xw))
    (print 2 "exolve-prelude:")
    (print 4 '("Replace with your prelude" "indented like this"))
    (print 2 "exolve-xw:")
    (print 4 (grid->lines (xword-grid xw)))
    (print 2 "exolve-across:")
    (print 4 (reverse ac))
    (print 2 "exolve-down:")
    (print 4 (reverse dn))
    (print 0 "exolve-end")))
  
(print-xw (parse-file "hh4.qxw"))
