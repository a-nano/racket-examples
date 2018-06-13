#lang racket
(require web-server/servlet
         web-server/servlet-env)
(require net/uri-codec
         net/base64)
(require file/convertible)

; serve files from this directory:
(define document-root
  (path->string (current-directory)))

; a table to map file extensions to MIME types:
(define ext=>mime-type
  #hash((#"" . #"text/html; charset=utf-8")
        (#"html" . #"text/html; charset=utf-8")
        (#"txt" . #"text/html; charset=utf-8")
        (#"png" . #"image/png")
        (#"jpeg" . #"image/jpeg")
        (#"rkt" . #"text/x-racket; charset=utf-8")))

;; pict -> String
(define (pict->data-uri pict)
  (format "data:image/*;base64,~a"　; image/png, image/jpeg
          (base64-encode pict)))

;; main-servlet
(define (main-servlet req)

  ; extract the URI from the request:
  (define uri (request-uri req))

  ; extract the resource from the URI:
  (define resource
    (map path/param-path (url-path uri)))

  (define page (car resource))

  ; find the file location
  (define file (string-append
                document-root
                "/"
                (string-join resource "/")))

  (cond

    ;;;;;;;;;;;;;;;;;;;
    ;; dynamic 
    ;;;;;;;;;;;;;;;;;;;
    
    ; /
    [(equal? page "")
     (response/xexpr
      `(html
        (body (p "現在web appのテスト中です。by 南尾")
              (p (a ([href "/form"]) ,(uri-decode "こちら"))
                 "も覗いてみてね！")
              (p (a ([href "/picture"]) ,(uri-decode "こっち"))
                 "には、写真アップロードテストもあるよ！"))))]

    ; /form
    [(equal? page "form")
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [action "/print-form-data"])
               "user name: " (input ([type "text"] [name "user"]))
               (br)
               "comment: " (input ([type "text"] [name "comment"]))
               (br)
               (input ([type "submit"]))))))]

    ; /print-form-data
    [(equal? page "print-form-data")

     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))

     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and coment:
     (define name (cdr (assq 'user form-data)))
     (define comment (cdr (assq 'comment form-data)))

     ; send back the extracted data:
     (response/xexpr
      `(html
        (body
         (p "Your name: " ,name)
         (p "Your comment: " ,comment))))]

    ; /picture
    [(equal? page "picture")
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [enctype "multipart/form-data"] [action "/print-picture-data"])
               (input ([type "file"] [name "pic"] [id "pic"] [accept "image/*"] [capture "camera"]))
               (br)
               (input ([type "submit"] [name "button"] [value "送信する"]))))))]

    ; /print-picture-data
    [(equal? page "print-picture-data")

     ; extract the post data:
     (define post-data (request-bindings/raw req))

     ; extract the pic data:
     (define pic-data (bindings-assq #"pic" post-data))

     ; extract the content data:
     (define content-data (binding:file-content pic-data))

     ; send back the extracted data:
     (response/xexpr
      `(html
        (body
         (p "アップされた写真")
         (img ([src ,(pict->data-uri content-data)])))))]


    ;;;;;;;;;;;;;;;;;;;
    ;; static files
    ;;;;;;;;;;;;;;;;;;;
    
    ; serve the file if it exists:
    [(file-exists? file)

     ; find the MIME type:
     (define extension (filename-extension file))
     (define mime-type
       (hash-ref ext=>mime-type extension
                 (lambda () TEXT/HTML-MIME-TYPE)))

     ; read the file contents:
     (define data (file->bytes file))

     ; construct the response
     (response
      200 #"Ok"
      (current-seconds)
      mime-type
      '()
      (lambda (client-out)
        (write-bytes data client-out)))]
    
    ; page not found
    [else
     (response/xexpr
      #:code 404
      #:message #"Not found"
      `(html
        (body
         (p "Page not found!"))))]))


(serve/servlet main-servlet
               #:port 80
               #:listen-ip #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:command-line? #t)

