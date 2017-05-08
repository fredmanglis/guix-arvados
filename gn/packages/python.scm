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

(define-public python-epydoc
  (package
  (name "python-epydoc")
  (version "3.0.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "epydoc" version))
      (sha256
        (base32
          "1wj8dqbybhc97ml29wyyzn6r7sfbrkbmvqrr5g26xc7safw6j568"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-setuptools" ,python-setuptools)))
  (arguments
   `(#:tests? #f))
  (home-page "http://epydoc.sourceforge.net")
  (synopsis "Tool for generating API documentation for Python modules")
  (description "@code{Epydoc} is a tool for generating API documentation for Python
modules, based on their docstrings.")
  (license l:expat)))
