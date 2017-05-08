;;; GNU Guix --- Functional Package management for GNU
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>

(define-module (gn packages python)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public python-linkchecker
  (package
  (name "python-linkchecker")
  (version "9.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "LinkChecker" version))
      (sha256
        (base32
          "0v8pavf0bx33xnz1kwflv0r7lxxwj7vg3syxhy2wzza0wh6sc2pf"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-setuptools" ,python-setuptools)))
  (arguments
   `(#:python ,python-2
     #:tests? #f))
  (home-page
    "http://wummel.github.io/linkchecker/")
  (synopsis "Check links in web documents or full websites")
  (description "@code{linkchecker} check for broken links in web documents or full
websites")
  (license l:gpl3)))
