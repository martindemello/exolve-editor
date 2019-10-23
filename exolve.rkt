#lang racket

(require "xword.rkt")
(require "qxw.rkt")

(provide (prefix-out exolve:
                     (combine-out merge extract-xw parse format-xw)))

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
  (let* [(data (xword-data xw))
         (ac (hash-ref data 'clues-across '()))
         (dn (hash-ref data 'clues-down '()))]
    (unlines
     (append
      (format 0 "exolve-begin")
      (format 2 (headers xw))
      (format 2 "exolve-prelude:")
      (format 4 '("Replace with your prelude"
                  "indented like this"))
      (format 2 "exolve-grid:")
      (format 4 (grid->lines (xword-grid xw)))
      (format 2 "exolve-across:")
      (format 4 ac)
      (format 2 "exolve-down:")
      (format 4 dn)
      (format 0 "exolve-end")))))

(define (merge template xword)
  (let* [(lines (string-split template "\n"))
         (prefix (takef lines (λ (l) (not (equal? l *start-marker*)))))
         (suffix (takef-right lines (λ (l) (not (equal? l *end-marker*)))))
         (replacement (list *start-marker* xword *end-marker*))]
    (unlines (append prefix replacement suffix))))

(define (extract-xw html)
  (let* [(lines (string-split html "\n"))
         (drop-prefix (dropf lines (λ (l) (not (equal? l *start-marker*)))))
         (xword (takef drop-prefix (λ (l) (not (equal? l *end-marker*)))))         ]
    (unlines (rest xword))))

(define (add-val h k v)
  (hash-set! h k (cons v (hash-ref h k '()))))

(define (parse-to-dict f)
  (let* [(dict (make-hash))
         (key #f)
         (add-line (λ (k v) (add-val dict k v)))
         (header-rx #px"exolve-begin|exolve-end|exolve-(.*):\\s*(.*)")]
    (for [(line (string-split f "\n"))]
      (let [(line (string-trim line))]
        (match (regexp-match header-rx line)
          [(list _ #f #f) #f] ; exolve-{begin,end}
          [(list _ k "") (set! key k)]
          [(list _ k v) (begin (set! key k) (hash-set! dict k v))]
          [#f (add-line key line)])))
    (make-hash
     (hash-map dict (λ (k v)
                      (cons k (if (list? v) (reverse v) v)))))))

(define (parse-dict dict)
  (let* [(h (λ (k) (hash-ref dict k)))
         (s string->number)
         (cols (h "width"))
         (rows (h "height"))
         (grid (h "grid"))
         (ac (h "across"))
         (dn (h "down"))
         (xw (make-xword (s cols) (s rows)))]
    (for [(line grid)
          (row (in-naturals))]
      (for [(c line)
            (col (in-naturals))]
        (match c
          [#\. (set-square xw col row ".")]
          [#\space (set-square xw col row "0")]
          [c (set-square xw col row (string c))])))
    (let [(data (xword-data xw))]
      (hash-set! data 'clues-across ac)
      (hash-set! data 'clues-down dn))
    xw))

(define (parse f)
  (parse-dict (parse-to-dict (extract-xw f))))