#lang racket
(require web-server/servlet
         web-server/servlet-env)
(require net/uri-codec
         net/base64)
(require file/convertible)

;; pict -> String
(define (pict->data-uri pict)
  (format "data:image/*;base64,~a"　; image/png, image/jpeg
          (base64-encode pict)))

;; main-servlet
(define (main-servlet req)

  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))
  (define page (car path))

  (cond
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
    
    ; page not found
    [else
     (response/xexpr
      `(html
        (body
         (p "Page not found!"))))]))


(serve/servlet main-servlet
               #:port 80
               #:listen-ip #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:command-line? #t)

