#lang racket

(require "xword.rkt")
(require "qxw.rkt")

(provide (prefix-out exolve:
                     (combine-out merge format-xw)))

(define *start-marker* "======REPLACE WITH YOUR PUZZLE BELOW======")
(define *end-marker* "======REPLACE WITH YOUR PUZZLE ABOVE======")

(define (indent n str)
  (string-append (build-string n (λ (_) #\space)) str))

(define (unlines list)
  (string-join list "\n"))

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

(define (format-light cells n)
  (let [(word (string-join cells ""))
        (num (number->string n))
        (enum (number->string (length cells)))]
    (string-append num ". " word " (" enum ") Annotation")))

(define (format ind line-or-lines)
  (cond [(list? line-or-lines) (map (λ (line) (indent ind line))
                                    line-or-lines)]
        [else (list (indent ind line-or-lines))]))

(define (format-xw xw)
  (let [(ac '())
        (dn '())
        (fmt (λ (fn i j n) (format-light (fn xw i j) n)))]
    (renumber xw
              #:on-ac (λ (i j n) (set! ac (cons (fmt word-ac+ i j n) ac)))
              #:on-dn (λ (i j n) (set! dn (cons (fmt word-dn+ i j n) dn))))   
    (let [(lines (append
                  (format 0 "exolve-begin")
                  (format 2 (headers xw))
                  (format 2 "exolve-prelude:")
                  (format 4 '("Replace with your prelude" "indented like this"))
                  (format 2 "exolve-xw:")
                  (format 4 (grid->lines (xword-grid xw)))
                  (format 2 "exolve-across:")
                  (format 4 (reverse ac))
                  (format 2 "exolve-down:")
                  (format 4 (reverse dn))
                  (format 0 "exolve-end")))]
      (unlines lines))))

(define (merge template xword)
  (let* [(lines (string-split template "\n"))
         (prefix (takef lines (λ (l) (not (equal? l *start-marker*)))))
         (suffix (takef-right lines (λ (l) (not (equal? l *end-marker*)))))
         (replacement (list *start-marker* xword *end-marker*))]
    (unlines (append prefix replacement suffix))))