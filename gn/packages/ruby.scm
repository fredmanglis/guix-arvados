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
   (propagated-inputs
    `(("ruby-progressbar" ,ruby-progressbar)))
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rubocop" ,ruby-rubocop)
      ("ruby-rainbow" ,ruby-rainbow)
      ("ruby-parser" ,ruby-parser)
      ("ruby-ast" ,ruby-ast)
      ("ruby-powerpack" ,ruby-powerpack)))
   (arguments
    `(#:tests? #f))
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
   (native-inputs
    `(("ruby-public-suffix" ,ruby-public-suffix)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
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

(define-public ruby-faraday
  (package
   (name "ruby-faraday")
   (version "0.12.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "faraday" version))
     (sha256
      (base32
       "1wkx9844vacsk2229xbc27djf6zw15kqd60ifr78whf9mp9v6l03"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-multipart-post" ,ruby-multipart-post)))
   (arguments
    ;; No Rakefile
    `(#:tests? #f))
   (synopsis "HTTP/REST API client library.")
   (description "HTTP/REST API client library.")
   (home-page
    "https://github.com/lostisland/faraday")
   (license l:expat)))

(define-public ruby-jwt
  (package
   (name "ruby-jwt")
   (version "1.5.6")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "jwt" version))
     (sha256
      (base32
       "124zz1142bi2if7hl5pcrcamwchv4icyr5kaal9m2q6wqbdl6aw4"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "A pure ruby implementation of the RFC 7519 OAuth JSON Web Token (JWT) standard.")
   (description
    "This package provides a pure ruby implementation of the RFC 7519 OAuth JSON Web Token (JWT) standard.")
   (home-page "http://github.com/jwt/ruby-jwt")
   (license l:expat)))

(define-public ruby-little-plugger
  (package
   (name "ruby-little-plugger")
   (version "1.1.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "little-plugger" version))
     (sha256
      (base32
       "1frilv82dyxnlg8k1jhrvyd73l6k17mxc5vwxx080r4x1p04gwym"))))
   (build-system ruby-build-system)
   (arguments
    ;; tests seem to require the bones gem, which has little-plugger
    ;; as a dependency
    `(#:tests? #f))
   (synopsis
    "LittlePlugger is a module that provides Gem based plugin management.
By extending your own class or module with LittlePlugger you can easily
manage the loading and initializing of plugins provided by other gems.")
   (description
    "LittlePlugger is a module that provides Gem based plugin management.
By extending your own class or module with LittlePlugger you can easily
manage the loading and initializing of plugins provided by other gems.")
   (home-page
    "http://gemcutter.org/gems/little-plugger")
   (license #f)))

(define-public ruby-multi-json
  (package
   (name "ruby-multi-json")
   (version "1.12.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "multi_json" version))
     (sha256
      (base32
       "1wpc23ls6v2xbk3l1qncsbz16npvmw8p0b38l8czdzri18mp51xk"))))
   (build-system ruby-build-system)
   (arguments
    ;; No Rakefile
    `(#:tests? #f))
   (synopsis
    "A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
   (description
    "This package provides a common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
   (home-page
    "http://github.com/intridea/multi_json")
   (license l:expat)))

(define-public ruby-loquacious
  (package
  (name "ruby-loquacious")
  (version "1.9.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "loquacious" version))
      (sha256
        (base32
          "0pg1k2rds6yp9gx1zg8pmfc2nb0vfmvidcq3pnc2qwwldvq3r5hz"))))
  (build-system ruby-build-system)
  (arguments
   ;; Circular dependency: The tests depend on bones, which
   ;; depends on this package
   `(#:tests? #f))
  (synopsis
    "Descriptive configuration files for Ruby written in Ruby.")
  (description
    "Descriptive configuration files for Ruby written in Ruby.

Loquacious provides a very open configuration system written in ruby and
descriptions for each configuration attribute.  The attributes and descriptions
can be iterated over allowing for helpful information about those attributes to
be displayed to the user.

In the simple case we have a file something like

  Loquacious.configuration_for('app') {
    name 'value', :desc => \"Defines the name\"
    foo  'bar',   :desc => \"FooBar\"
    id   42,      :desc => \"Ara T.  Howard\"
  }

Which can be loaded via the standard Ruby loading mechanisms

  Kernel.load 'config/app.rb'

The attributes and their descriptions can be printed by using a Help object

  help = Loquacious.help_for('app')
  help.show :values => true        # show the values for the attributes, too

Descriptions are optional, and configurations can be nested arbitrarily deep.

  Loquacious.configuration_for('nested') {
    desc \"The outermost level\"
    a {
      desc \"One more level in\"
      b {
        desc \"Finally, a real value\"
        c 'value'
      }
    }
  }

  config = Loquacious.configuration_for('nested')

  p config.a.b.c  #=> \"value\"

And as you can see, descriptions can either be given inline after the value or
they can appear above the attribute and value on their own line.")
  (home-page "http://rubygems.org/gems/loquacious")
  (license #f)))

(define-public ruby-rdoc
  (package
   (name "ruby-rdoc")
   (version "5.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rdoc" version))
     (sha256
      (base32
       "1p4ldqkndkk0a6nqpnd9x8p7vgirmfw8cdhfc1j6vz4sbc390l0r"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis
    "RDoc produces HTML and command-line documentation for Ruby projects.
RDoc includes the +rdoc+ and +ri+ tools for generating and displaying documentation from the command-line.
")
   (description
    "RDoc produces HTML and command-line documentation for Ruby projects.
RDoc includes the +rdoc+ and +ri+ tools for generating and displaying documentation from the command-line.
")
   (home-page "https://rdoc.github.io/rdoc")
   (license #f)))

(define-public ruby-bones
  (package
   (name "ruby-bones")
   (version "3.8.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bones" version))
     (sha256
      (base32
       "1diyv7k9i14fdrs80mab2myczrhnq39gjzvfwpd7z7fzw7vh8yrl"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-little-plugger" ,ruby-little-plugger)
      ("ruby-loquacious" ,ruby-loquacious)
      ("ruby-rake" ,ruby-rake)
      ("ruby-rdoc" ,ruby-rdoc)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "Mr Bones is a handy tool that creates new Ruby projects from a code
skeleton. The skeleton contains some starter code and a collection of rake
tasks to ease the management and deployment of your source code. Several Mr
Bones plugins are available for creating git repositories, creating GitHub
projects, running various test suites and source code analysis tools.")
   (description
    "Mr Bones is a handy tool that creates new Ruby projects from a code
skeleton.  The skeleton contains some starter code and a collection of rake
tasks to ease the management and deployment of your source code.  Several Mr
Bones plugins are available for creating git repositories, creating GitHub
projects, running various test suites and source code analysis tools.")
   (home-page "http://rubygems.org/gems/bones")
   (license #f)))

(define-public ruby-logging
  (package
   (name "ruby-logging")
   (version "2.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "logging" version))
     (sha256
      (base32
       "06j6iaj89h9jhkx1x3hlswqrfnqds8br05xb1qra69dpvbdmjcwn"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-little-plugger" ,ruby-little-plugger)
      ("ruby-multi-json" ,ruby-multi-json)
      ("ruby-bones" ,ruby-bones)))
   (arguments
    ;; Failing test. Fix it if possible.
    `(#:tests? #f))
   (synopsis
    "**Logging** is a flexible logging library for use in Ruby programs based on the
design of Java's log4j library. It features a hierarchical logging system,
custom level names, multiple output destinations per log event, custom
formatting, and more.")
   (description
    "**Logging** is a flexible logging library for use in Ruby programs based on the
design of Java's log4j library.  It features a hierarchical logging system,
custom level names, multiple output destinations per log event, custom
formatting, and more.")
   (home-page "http://rubygems.org/gems/logging")
   (license #f)))

(define-public ruby-memoist
  (package
   (name "ruby-memoist")
   (version "0.16.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "memoist" version))
     (sha256
      (base32
       "0pq8fhqh8w25qcw9v3vzfb0i6jp0k3949ahxc3wrwz2791dpbgbh"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis "memoize methods invocation")
   (description "memoize methods invocation")
   (home-page
    "https://github.com/matthewrudy/memoist")
   (license l:expat)))

(define-public ruby-os
  (package
   (name "ruby-os")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "os" version))
     (sha256
      (base32
       "1s401gvhqgs2r8hh43ia205mxsy1wc0ib4k76wzkdpspfcnfr1rk"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "The OS gem allows for some useful and easy functions, like OS.windows? (=&gt; true or false) OS.bits ( =&gt; 32 or 64) etc\"")
   (description
    "The OS gem allows for some useful and easy functions, like OS.windows? (=&gt; true or false) OS.bits ( =&gt; 32 or 64) etc\"")
   (home-page "http://github.com/rdp/os")
   (license #f)))

(define-public ruby-signet
  (package
   (name "ruby-signet")
   (version "0.7.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "signet" version))
     (sha256
      (base32
       "149668991xqibvm8kvl10kzy891yd6f994b4gwlx6c3vl24v5jq6"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-addressable" ,ruby-addressable)
      ("ruby-faraday" ,ruby-faraday)
      ("ruby-jwt" ,ruby-jwt)
      ("ruby-multi-json" ,ruby-multi-json)
      ("ruby-yard" ,ruby-yard)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "Signet is an OAuth 1.0 / OAuth 2.0 implementation.
")
   (description
    "Signet is an OAuth 1.0 / OAuth 2.0 implementation.
")
   (home-page "https://github.com/google/signet/")
   (license #f)))

(define-public ruby-googleauth
  (package
   (name "ruby-googleauth")
   (version "0.5.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "googleauth" version))
     (sha256
      (base32
       "1nzkg63s161c6jsia92c1jfwpayzbpwn588smd286idn07y0az2m"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-faraday" ,ruby-faraday)
      ("ruby-jwt" ,ruby-jwt)
      ("ruby-logging" ,ruby-logging)
      ("ruby-memoist" ,ruby-memoist)
      ("ruby-multi-json" ,ruby-multi-json)
      ("ruby-os" ,ruby-os)
      ("ruby-signet" ,ruby-signet)
      ("ruby-rspec" ,ruby-rspec)
      ("ruby-rubocop" ,ruby-rubocop)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "   Allows simple authorization for accessing Google APIs.
   Provide support for Application Default Credentials, as described at
   https://developers.google.com/accounts/docs/application-default-credentials
")
   (description
    "   Allows simple authorization for accessing Google APIs.
   Provide support for Application Default Credentials, as described at
   https://developers.google.com/accounts/docs/application-default-credentials
")
   (home-page
    "https://github.com/google/google-auth-library-ruby")
   (license #f)))

(define-public ruby-httpclient
  (package
   (name "ruby-httpclient")
   (version "2.8.3")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "httpclient" version))
     (sha256
      (base32
       "19mxmvghp7ki3klsxwrlwr431li7hm1lczhhj8z4qihl2acy8l99"))))
   (build-system ruby-build-system)
   (arguments
    ;; No Rakefile
    `(#:tests? #f))
   (synopsis
    "gives something like the functionality of libwww-perl (LWP) in Ruby")
   (description
    "gives something like the functionality of libwww-perl (LWP) in Ruby")
   (home-page "https://github.com/nahi/httpclient")
   (license #f)))

(define-public ruby-declarative
  (package
   (name "ruby-declarative")
   (version "0.0.9")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "declarative" version))
     (sha256
      (base32
       "1b2vjfkp8wfvmwmfhq5bpw9igcl85ry9gaf8341d8z7gz4z4188j"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis
    "DSL for nested generic schemas with inheritance and refining.")
   (description
    "DSL for nested generic schemas with inheritance and refining.")
   (home-page "")
   (license l:expat)))

(define-public ruby-declarative-option
  (package
   (name "ruby-declarative-option")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "declarative-option" version))
     (sha256
      (base32
       "1g4ibxq566f1frnhdymzi9hxxcm4g2gw4n21mpjk2mhwym4q6l0p"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (synopsis "Dynamic options.")
   (description "Dynamic options.")
   (home-page
    "https://github.com/apotonick/declarative-option")
   (license l:expat)))

(define-public ruby-benchmark-ips
  (package
   (name "ruby-benchmark-ips")
   (version "2.7.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "benchmark-ips" version))
     (sha256
      (base32
       "1w59c4qnwkjqwn7zyp9hshslbshna77vknmz43h0va5lxisd6ai2"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-hoe" ,ruby-hoe)))
   (synopsis
    "An iterations per second enhancement to Benchmark.")
   (description
    "An iterations per second enhancement to Benchmark.")
   (home-page
    "https://github.com/evanphx/benchmark-ips")
   (license l:expat)))

(define-public ruby-uber
  (package
   (name "ruby-uber")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "uber" version))
     (sha256
      (base32
       "1p1mm7mngg40x05z52md3mbamkng0zpajbzqjjwmsyw0zw3v9vjv"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)
      ("benchmark-ips" ,ruby-benchmark-ips)))
   (arguments
    ;; Some tests are failing due to errors.
    `(#:tests? #f))
   (synopsis "A gem-authoring framework.")
   (description
    "This package provides a gem-authoring framework.")
   (home-page "https://github.com/apotonick/uber")
   (license l:expat)))

(define-public ruby-diffy
  (package
   (name "ruby-diffy")
   (version "3.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "diffy" version))
     (sha256
      (base32
       "015nn9zaciqj43mfpjlw619r5dvnfkrjcka8nsa6j260v6qya941"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(;; Don't know how to build task 'test'
      #:tests? #f))
   (synopsis "Convenient diffing in ruby")
   (description "Convenient diffing in ruby")
   (home-page "http://github.com/samg/diffy")
   (license l:expat)))

(define-public ruby-test-xml
  (package
   (name "ruby-test-xml")
   (version "0.1.8")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "test_xml" version))
     (sha256
      (base32
       "1p2whnl8ghjpjkckvzqkm0rb04qad94qd07kk7qlywd2g62z8czj"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-diffy" ,ruby-diffy)
      ("ruby-nokogiri" ,ruby-nokogiri)
      ("bundler" ,bundler)
      ("ruby-rspec", ruby-rspec)))
   (synopsis
    "Test your XML with Test::Unit, MiniTest, RSpec, or Cucumber using handy assertions like #assert_xml_equal or #assert_xml_structure_contain.")
   (description
    "Test your XML with Test::Unit, MiniTest, RSpec, or Cucumber using handy assertions like #assert_xml_equal or #assert_xml_structure_contain.")
   (home-page "http://github.com/alovak/test_xml")
   (license l:expat)))

(define-public ruby-axiom-types
  (package
   (name "ruby-axiom-types")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "axiom-types" version))
     (sha256
      (base32
       "10q3k04pll041mkgy0m5fn2b1lazm6ly1drdbcczl5p57lzi3zy1"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-descendants-tracker"
       ,ruby-descendants-tracker)
      ("ruby-ice-nine" ,ruby-ice-nine)
      ("ruby-thread-safe" ,ruby-thread-safe)))
   (arguments
    `(;; Define packages commented out above#:tests? #f
      ))
   (synopsis
    "Define types with optional constraints for use within axiom and other libraries.")
   (description
    "Define types with optional constraints for use within axiom and other libraries.")
   (home-page
    "https://github.com/dkubb/axiom-types")
   (license l:expat)))

(define-public ruby-virtus
  (package
   (name "ruby-virtus")
   (version "1.0.5")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "virtus" version))
     (sha256
      (base32
       "06iphwi3c4f7y9i2rvhvaizfswqbaflilziz4dxqngrdysgkn1fk"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-axiom-types" ,ruby-axiom-types)
      ("ruby-coercible" ,ruby-coercible)
      ("ruby-descendants-tracker"
       ,ruby-descendants-tracker)
      ("ruby-equalizer" ,ruby-equalizer)))
   (synopsis
    "Attributes on Steroids for Plain Old Ruby Objects")
   (description
    "Attributes on Steroids for Plain Old Ruby Objects")
   (home-page "https://github.com/solnic/virtus")
   (license l:expat)))

(define-public ruby-representable
  (package
   (name "ruby-representable")
   (version "3.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "representable" version))
     (sha256
      (base32
       "0qm9rgi1j5a6nv726ka4mmixivlxfsg91h8rpp72wwd4vqbkkm07"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-declarative" ,ruby-declarative)
      ("ruby-declarative-option" ,ruby-declarative-option)
      ("ruby-uber" ,ruby-uber)
      ("bundler" ,bundler)
      ("ruby-test-xml" ,ruby-test-xml)))
   (arguments
    `(;; Define remaining packages and fix any issues
      #:tests? #f))
   (synopsis
    "Renders and parses JSON/XML/YAML documents from and to Ruby objects. Includes plain properties, collections, nesting, coercion and more.")
   (description
    "Renders and parses JSON/XML/YAML documents from and to Ruby objects.  Includes plain properties, collections, nesting, coercion and more.")
   (home-page
    "https://github.com/trailblazer/representable/")
   (license l:expat)))

(define-public ruby-numerizer
  (package
   (name "ruby-numerizer")
   (version "0.2.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "numerizer" version))
     (sha256
      (base32
       "0ysxf30qcybh131r98frp38sqqkdhcjwpnajgrxl2w2kxvapd075"))))
   (build-system ruby-build-system)
   (synopsis
    "Numerizer is a gem to help with parsing numbers in natural language from strings (ex forty two). It was extracted from the awesome Chronic gem http://github.com/evaryont/chronic.")
   (description
    "Numerizer is a gem to help with parsing numbers in natural language from strings (ex forty two).  It was extracted from the awesome Chronic gem http://github.com/evaryont/chronic.")
   (home-page "http://github.com/jduff/numerizer")
   (license l:expat)))

(define-public ruby-chronic-duration
  (package
   (name "ruby-chronic-duration")
   (version "0.10.6")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "chronic_duration" version))
     (sha256
      (base32
       "1k7sx3xqbrn6s4pishh2pgr4kw6fmw63h00lh503l66k8x0qvigs"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-numerizer" ,ruby-numerizer)
      ("bundler" ,bundler)
      ("ruby-rspec" ,ruby-rspec)))
   (arguments
    `(#:tests? #f))
   (synopsis
    "A simple Ruby natural language parser for elapsed time. (For example, 4 hours and 30 minutes, 6 minutes 4 seconds, 3 days, etc.) Returns all results in seconds. Will return an integer unless you get tricky and need a float. (4 minutes and 13.47 seconds, for example.) The reverse can also be performed via the output method.")
   (description
    "This package provides a simple Ruby natural language parser for elapsed time. (For example, 4 hours and 30 minutes, 6 minutes 4 seconds, 3 days, etc.) Returns all results in seconds.  Will return an integer unless you get tricky and need a float. (4 minutes and 13.47 seconds, for example.) The reverse can also be performed via the output method.")
   (home-page
    "https://github.com/hpoydar/chronic_duration")
   (license l:expat)))

(define-public ruby-spec
  (package
   (name "ruby-spec")
   (version "5.3.4")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "spec" version))
     (sha256
      (base32
       "19ghxr78ddz6apnph5wyz3clykcbi7d3wbrqzc2cx2p1asci1niz"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("ruby-chronic-duration" ,ruby-chronic-duration)
      ("ruby-hoe" ,ruby-hoe)))
   (arguments
    `(;; Failing tests: maybe remove them?
      #:tests? #f))
   (synopsis "Modified minitest for Appium.")
   (description "Modified minitest for Appium.")
   (home-page
    "https://github.com/bootstraponline/spec")
   (license l:expat)))

(define-public ruby-codeclimate-test-reporter
  (package
   (name "ruby-codeclimate-test-reporter")
   (version "1.0.8")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri
	   "codeclimate-test-reporter"
	   version))
     (sha256
      (base32
       "11k4kwkg22lpgr25i33h0mspz2yaxszss1z6i8sjl5w8xma0m9ap"))))
   (build-system ruby-build-system)
   (arguments
    `(;; No Rakefile
      #:tests? #f))
   (native-inputs
    `(("ruby-simplecov" ,ruby-simplecov)))
   (synopsis
    "Collects test coverage data from your Ruby test suite and sends it to Code Climate's hosted, automated code review service. Based on SimpleCov.")
   (description
    "Collects test coverage data from your Ruby test suite and sends it to Code Climate's hosted, automated code review service.  Based on SimpleCov.")
   (home-page
    "https://github.com/codeclimate/ruby-test-reporter")
   (license l:expat)))

(define-public ruby-retriable
  (package
   (name "ruby-retriable")
   (version "3.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "retriable" version))
     (sha256
      (base32
       "1lwprl0n07ybxfsfscji282dibw7byp26mz6xs08a0rprf0hl8xy"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-spec" ,ruby-spec)
      ("ruby-codeclimate-test-reporter" ,ruby-codeclimate-test-reporter)
      ("ruby-simplecov" ,ruby-simplecov)
      ("ruby-minitest" ,ruby-minitest)
      ("ruby-minitest-focus" ,ruby-minitest-focus)
      ("ruby-pry" ,ruby-pry)))
   (synopsis
    "Retriable is an simple DSL to retry failed code blocks with randomized exponential backoff. This is especially useful when interacting external api/services or file system calls.")
   (description
    "Retriable is an simple DSL to retry failed code blocks with randomized exponential backoff.  This is especially useful when interacting external api/services or file system calls.")
   (home-page "http://github.com/kamui/retriable")
   (license l:expat)))

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
      ;;("ruby-mime-types" ,ruby-mime-types)
      ("ruby-representable" ,ruby-representable)
      ("ruby-retriable" ,ruby-retriable)
      ("bundler" ,bundler)))
   (arguments
    `(;; mime-types fails to build successfully
      ;; no such file: rubocop/rake_task
      #:tests? #f))
   (synopsis "Client for accessing Google APIs")
   (description "Client for accessing Google APIs")
   (home-page
    "https://github.com/google/google-api-ruby-client")
   (license #f)))
