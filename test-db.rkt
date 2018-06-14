#lang racket
(require db)
(require net/base64)

(define test-db (sqlite3-connect #:database "test.db"))

(define (read-image file)
  (call-with-input-file file
    (lambda (in)
      (read-bytes (read-byte in) in))))

;; bstr -> string
(define (encomp-image bstr)
  (bytes->string/utf-8 (base64-encode bstr)))

;; string -> bstr
(define (decomp-image str)
  (base64-decode (string->bytes/utf-8 str)))


(define (insert-db conn image filename project comment)
  (query-exec conn
              (format "insert into photos values (NULL, '~A', '~A', ~A, '~A');"
                      (encomp-image image) filename project comment)))

(define (delete-db conn project)
  (query-exec conn
              (format "delete from photos where project = ~A"
                      project)))

(define (select-db conn project)
  (query-rows conn
              (format "select * from photos where project = ~A"
                      project)))