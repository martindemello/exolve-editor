#lang racket

(require "xword.rkt")

(provide parse-file)

(define (parse-list lines)
  (let* [(xw #f)
         (s string->number)]
    (for [(line lines)]
      (match (string-split line)
        [(list "GP" "0" w h _ _ _) (set! xw (make-xword (s w) (s h)))]
        [(list "SQ" col row _ _ "1") (set-square xw (s col) (s row) ".")]
        [(list "SQ" col row _ _ "0") (set-square xw (s col) (s row) "0")]
        [(list "SQ" col row _ _ "0" c) (set-square xw (s col) (s row) c)]
        [_ #f]))
    xw))

(define (parse-file f)
  (parse-list (file->lines f)))
