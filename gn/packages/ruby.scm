;;; GNU Guix --- Functional Package management for GNU
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>

(define-module (gn packages ruby)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages))

(define-public ruby-rake
  (package
   (name "ruby-rake")
   (version "12.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rake" version))
     (sha256
      (base32
       "01j8fc9bqjnrsxbppncai05h43315vmz9fwg28qdsgcjw9ck1d7n"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis
    "Rake is a Make-like program implemented in Ruby")
   (description
    "@code{Rake} is a Make-like program implemented in @code{Ruby}.  Tasks and
dependencies are specified in standard @code{Ruby} syntax.
Rake has the following features:
@enumerate
@item Rakefiles (rake's version of Makefiles) are completely defined in standard
@code{Ruby} syntax.  No XML files to edit.  No quirky Makefile syntax to worry
about (is that a tab or a space?)
@item Users can specify tasks with prerequisites.
@item Rake supports rule patterns to synthesize implicit tasks.
@item Flexible FileLists that act like arrays but know about manipulating file
names and paths.
@item Supports parallel execution of tasks.
@end enumerate")
   (home-page "https://github.com/ruby/rake")
   (license l:expat)))

(define-public ruby-parallel
  (package
   (name "ruby-parallel")
   (version "1.11.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "parallel" version))
     (sha256
      (base32
       "1yxhk64sfkm84v509m3mzmp0b7ra9lc7s8k6113yk9faih3g7w7v"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f))
   (synopsis
    "Run any kind of code in parallel processes")
   (description
    "Run any kind of code in parallel processes")
   (home-page "https://github.com/grosser/parallel")
   (license l:expat)))

(define-public ruby-bacon-colored-output
  (package
  (name "ruby-bacon-colored-output")
  (version "1.1.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "bacon-colored_output" version))
      (sha256
        (base32
          "1znyh3vkfdlmf19p3k4zip88ibym41dn5g4p4n5hmks2iznb7qpx"))))
  (build-system ruby-build-system)
  (native-inputs
   `(("ruby-bacon" ,ruby-bacon)
     ("bundler" ,bundler)))
  (arguments
   `(#:tests? #f))
  (synopsis
    "Colored output for Bacon test framework! http://i.imgur.com/EpTpw.png")
  (description
    "Colored output for Bacon test framework! http://i.imgur.com/EpTpw.png")
  (home-page "")
  (license #f)))

(define-public ruby-coveralls
  (package
   (name "ruby-coveralls")
   (version "0.8.21")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "coveralls" version))
     (sha256
      (base32
       "0akifzdykdbjlawkk3vbc9pxrw76g7dz5g9ankrvq8xhbw4crdnv"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-json" ,ruby-json)
      ("ruby-simplecov" ,ruby-simplecov)
      ("ruby-term-ansicolor" ,ruby-term-ansicolor)
      ("ruby-thor" ,ruby-thor)
      ("ruby-tins" ,ruby-tins)
      ("bundler" ,bundler)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "A Ruby implementation of the Coveralls API.")
   (description
    "This package provides a Ruby implementation of the Coveralls API.")
   (home-page "https://coveralls.io")
   (license l:expat)))

(define-public ruby-ast
  (package
   (name "ruby-ast")
   (version "2.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "ast" version))
     (sha256
      (base32
       "0pp82blr5fakdk27d1d21xq9zchzb6vmyb1zcsl520s3ygvprn8m"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rake" ,ruby-rake)
      ("ruby-bacon" ,ruby-bacon)
      ("ruby-bacon-colored-output" ,ruby-bacon-colored-output)
      ("ruby-simplecov" ,ruby-simplecov)
      ("ruby-coveralls" ,ruby-coveralls)
      ;;("ruby-json-pure" ,ruby-json-pure)
      ))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases
       %standard-phases
       (add-before
	'check
	'update-dependencies
	(lambda _
	  (substitute* "ast.gemspec"
		       (("~> 10.0") "~> 12.0")))))))
   (synopsis
    "A library for working with Abstract Syntax Trees.")
   (description
    "This package provides a library for working with Abstract Syntax Trees.")
   (home-page "https://whitequark.github.io/ast/")
   (license l:expat)))

(define-public ruby-parser
  (package
   (name "ruby-parser")
   (version "2.4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "parser" version))
     (sha256
      (base32
       "130rfk8a2ws2fyq52hmi1n0xakylw39wv4x1qhai4z17x2b0k9cq"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-ast" ,ruby-ast)
      ("bundler" ,bundler)))
   (arguments
    `(#:tests? #f))
   (synopsis "A Ruby parser written in pure Ruby.")
   (description
    "This package provides a Ruby parser written in pure Ruby.")
   (home-page
    "https://github.com/whitequark/parser")
   (license l:expat)))

(define-public ruby-powerpack
  (package
   (name "ruby-powerpack")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "powerpack" version))
     (sha256
      (base32
       "1fnn3fli5wkzyjl4ryh0k90316shqjfnhydmc7f8lqpi0q21va43"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "A few useful extensions to core Ruby classes.")
   (description
    "This package provides a few useful extensions to core Ruby classes.")
   (home-page
    "https://github.com/bbatsov/powerpack")
   (license l:expat)))

(define-public ruby-rainbow
  (package
   (name "ruby-rainbow")
   (version "2.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rainbow" version))
     (sha256
      (base32
       "08w2ghc5nv0kcq5b257h7dwjzjz1pqcavajfdx2xjyxqsvh2y34w"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-rake" ,ruby-rake)
      ("bundler" ,bundler)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "Colorize printed text on ANSI terminals")
   (description
    "Colorize printed text on ANSI terminals")
   (home-page "https://github.com/sickill/rainbow")
   (license l:expat)))

(define-public ruby-progressbar
  (package
   (name "ruby-progressbar")
   (version "1.8.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "progressbar" version))
     (sha256
      (base32
       "15zs2a18v5z28y6bxzrljbd8dcpkf86qw4k62vcchjzqlhklsfai"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.")
   (description
    "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.
The output can be customized with a flexible formatting system including:
percentage, bars of various formats, elapsed time and estimated time remaining.")
   (home-page "https://github.com/jfelchner/ruby-progressbar")
   (license l:expat)))

(define-public ruby-unicode-emoji
  (package
   (name "ruby-unicode-emoji")
   (version "0.9.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "unicode-emoji" version))
     (sha256
      (base32
       "120wa4mdn6r29y8iwfc91ksazr44dfka2k0m7nwwqhv1gsb72q6i"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f))
   (synopsis
    "[Emoji 5.0] Retrieve emoji data about Unicode codepoints. Also contains a regex to match emoji.")
   (description
    "[Emoji 5.0] Retrieve emoji data about Unicode codepoints.  Also contains a regex to match emoji.")
   (home-page
    "https://github.com/janlelis/unicode-emoji")
   (license l:expat)))

(define-public ruby-unicode-display-width
  (package
   (name "ruby-unicode-display-width")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "unicode-display_width" version))
     (sha256
      (base32
       "12pi0gwqdnbx1lv5136v3vyr0img9wr0kxcn4wn54ipq4y41zxq8"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-rspec" ,ruby-rspec)
      ("ruby-unicode-emoji" ,ruby-unicode-emoji)))
   (synopsis
    "[Unicode 10.0.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
   (description
    "[Unicode 10.0.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
   (home-page
    "http://github.com/janlelis/unicode-display_width")
   (license l:expat)))

(define-public ruby-rubocop
  (package
   (name "ruby-rubocop")
   (version "0.49.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rubocop" version))
     (sha256
      (base32
       "1mqyylfzch0967w7nfnqza84sqhljijd9y4bnq8hcmrscch75cxw"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-parallel" ,ruby-parallel)
      ("ruby-parser" ,ruby-parser)
      ("ruby-powerpack" ,ruby-powerpack)
      ("ruby-rainbow" ,ruby-rainbow)
      ("ruby-progressbar" ,ruby-progressbar)
      ("ruby-unicode-display-width"
       ,ruby-unicode-display-width)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.")
   (description
    "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.")
   (home-page "http://github.com/bbatsov/rubocop")
   (license l:expat)))

(define-public ruby-public-suffix
  (package
   (name "ruby-public-suffix")
   (version "2.0.5")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "public_suffix" version))
     (sha256
      (base32
       "040jf98jpp6w140ghkhw2hvc1qx41zvywx5gj7r2ylr1148qnj7q"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis
    "PublicSuffix can parse and decompose a domain name into top level domain, domain and subdomains.")
   (description
    "PublicSuffix can parse and decompose a domain name into top level domain, domain and subdomains.")
   (home-page
    "https://simonecarletti.com/code/publicsuffix-ruby")
   (license l:expat)))

(define-public ruby-addressable
  (package
   (name "ruby-addressable")
   (version "2.5.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "addressable" version))
     (sha256
      (base32
       "1i8q32a4gr0zghxylpyy7jfqwxvwrivsxflg9mks6kx92frh75mh"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-public-suffix" ,ruby-public-suffix)))
   (synopsis
    "Addressable is a replacement for the URI implementation that is part of
Ruby's standard library. It more closely conforms to the relevant RFCs and
adds support for IRIs and URI templates.
")
   (description
    "Addressable is a replacement for the URI implementation that is part of
Ruby's standard library.  It more closely conforms to the relevant RFCs and
adds support for IRIs and URI templates.
")
   (home-page
    "https://github.com/sporkmonger/addressable")
   (license #f)))

(define-public ruby-google-api-client
  (package
   (name "ruby-google-api-client")
   (version "0.13.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "google-api-client" version))
     (sha256
      (base32
       "05aklirdmhpc4cskzajrzbhlvzramc0mv8fb5w50l3cja4lph9ng"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-addressable" ,ruby-addressable)
      ("ruby-googleauth" ,ruby-googleauth)
      ("ruby-httpclient" ,ruby-httpclient)
      ("ruby-mime-types" ,ruby-mime-types)
      ("ruby-representable" ,ruby-representable)
      ("ruby-retriable" ,ruby-retriable)))
   (synopsis "Client for accessing Google APIs")
   (description "Client for accessing Google APIs")
   (home-page
    "https://github.com/google/google-api-ruby-client")
   (license #f)))
