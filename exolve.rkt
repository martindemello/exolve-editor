#lang racket

(require "xword.rkt")
(require "qxw.rkt")

(provide (prefix-out exolve:
                     (combine-out merge extract-xw parse-xw parse format-sections format-xw)))

(define *start-marker* "======REPLACE WITH YOUR PUZZLE BELOW======")
(define *end-marker* "======REPLACE WITH YOUR PUZZLE ABOVE======")

(define *sections* '(id title setter copyright prelude width height grid
                        across down nodir explanations nina colour question
                        submit option))

; keys treated specially by the code
(define *special-keys* (list->set '(width height grid across down)))

(define *defaults*
  (make-hash (list
              '(id . "unique-id")
              '(title . "Title")
              '(setter . "Setter")
              '(copyright . "year Name")
              '('prelude "Replace with your prelude" "indented like this"))))

(define (indent n str)
  (string-append (build-string n (λ (_) #\space)) str))

(define (unlines list)
  (string-join list "\n"))

(define (row->str row)
  (string-join (vector->list row) ""))

(define (grid->lines grid)
  (vector->list (vector-map row->str grid)))

(define (format-key k)
  (format "exolve-~a:" k))

(define (indent-lines ind v)
  (map (λ (line) (indent ind line)) v))

(define (format-kv k v)
  (let [(k (format-key k))]
    (cond [(list? v) (append (list (indent 2 k))
                             (indent-lines 4 v))]
          [else (list (indent 2 (format "~a ~a" k v)))])))

(define (get-value xw k)
  (let* [(data (xword-data xw))]
    (match k
      ['width (number->string (xword-cols xw))]
      ['height (number->string (xword-rows xw))]
      ['across (hash-ref data 'clues-across #f)]
      ['down (hash-ref data 'clues-down #f)]
      ['grid (grid->lines (xword-grid xw))]
      [_ (hash-ref data k (hash-ref *defaults* k #f))])))

(define (format-sections xw)
  (apply append
         (for/list [(k *sections*)]
           (let [(v (get-value xw k))]
             (cond
               [(list? v) (format-kv k v)]
               [(false? v) '()]
               ['else (format-kv k v)])))))

(define (format-xw xw)
  (let* [(data (xword-data xw))
         (ac (hash-ref data 'clues-across '()))
         (dn (hash-ref data 'clues-down '()))]
    (unlines
     (append
      '("exolve-begin")
      (format-sections xw)
      '("exolve-end")))))

(define title-rx #px"<title>(.*)</title>")

(define (find-title lines)
  (index-where lines (λ (l) (regexp-match title-rx l))))

(define (get-title lines)
  (let [(ix (find-title lines))]
    (if ix
        (let* [(ttl (list-ref lines ix))
               (m (regexp-match title-rx (first ttl)))]
          (second m))
        #f)))

(define (set-title lines title)
  (let [(ix (find-title lines))]
    (if ix
        (let* [(prefix (take lines ix))
               (suffix (drop lines (+ ix 1)))
               (ttl (format "<title>~a</title>" title))]
          (append prefix (cons ttl suffix)))
        lines)))

(define (merge template xw)
  (let* [(lines (string-split template "\n"))
         (prefix (takef lines (λ (l) (not (equal? l *start-marker*)))))
         (suffix (takef-right lines (λ (l) (not (equal? l *end-marker*)))))
         (xword (format-xw xw))
         (replacement (list *start-marker* xword *end-marker*))
         (out (append prefix replacement suffix))
         (title (get-data xw 'title))]
    (unlines (if title (set-title out title) out))))

(define (extract-xw html)
  (let* [(lines (string-split html "\n"))
         (drop-prefix (dropf lines (λ (l) (not (equal? l *start-marker*)))))
         (xword (takef drop-prefix (λ (l) (not (equal? l *end-marker*)))))]
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
                      (cons (string->symbol k)
                            (if (list? v) (reverse v) v)))))))

(define (parse-dict dict)
  (let* [(h (λ (k) (hash-ref dict k)))
         (s string->number)
         (cols (h 'width))
         (rows (h 'height))
         (grid (h 'grid))
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
      (hash-set! data 'clues-across (h 'across))
      (hash-set! data 'clues-down (h 'down))
      (hash-for-each dict (λ (k v)
                            (or (set-member? *special-keys* k)
                                (hash-set! data k v)))))
    xw))

(define (parse-xw xw)
  (parse-dict (parse-to-dict xw)))

(define (parse f)
  (parse-xw (extract-xw f)))
