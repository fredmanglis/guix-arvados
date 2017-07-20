;;; GNU Guix --- Functional Package management for GNU
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>

(define-module (gn packages arvados)
  #:use-module (gn packages python)
  #:use-module (gn packages ruby)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages))

(define (build-make-dirs dirs)
  `(lambda* _
     (let* ((cwd (getcwd))
	    (src_dir (string-append cwd "/gopath/src")))
       (do ((i ,dirs (cdr i)))
	   ((null? i))
	 (mkdir-p (string-append src_dir (car i)))))))

(define (build-setup-go-workspace dirs)
  `(add-before
    'unpack
    'setup-go-workspace
    ,(build-make-dirs dirs)))

(define (use-url-fetch name url hash)
  `(,name
    ,(origin
      (method url-fetch)
      (uri url)
      (sha256
       (base32
	hash)))))

(define (use-git-fetch name uri commit-hash hash)
  `(,name
    ,(origin
      (method git-fetch)
      (uri
       (git-reference
	(url uri)
	(commit commit-hash)))
      (sha256
       (base32
	hash)))))

(define (build-yaml-src)
  (use-url-fetch
   "yaml-src"
   "https://github.com/ghodss/yaml/archive/v1.0.0.tar.gz"
   "0i6yjwh3j2184lwwi537q7z666wppf5s1kz1m894d53is5yb8xla"))

(define (build-yaml.v2-src)
  (use-git-fetch
   "yaml.v2-src"
   ;; The url below gets the repo and checks out the
   ;; correct branch
   "https://gopkg.in/yaml.v2"
   "cd8b52f8269e0feb286dfeef29f8fe4d5b397e0b"
   "1hj2ag9knxflpjibck0n90jrhsrqz7qvad4qnif7jddyapi9bqzl"))

(define (build-goamz-src)
  (use-git-fetch
   "goamz-src"
   "https://github.com/AdRoll/goamz.git"
   "c5d7d9bd6c743fae44efc6c18450282022445ffc"
   "0yj1yqnskp1zcmihiq3yxlb9gm3w8ng5radvzjnc60r8vc78ihg2"))

(define (build-go-systemd-src)
  (use-url-fetch
   "go-systemd-src"
   "https://github.com/coreos/go-systemd/archive/v14.tar.gz"
   "0219hmzbyvrlbarbrs66hpc076hi0hfbk6invvmzrp5x4lda96wx"))

(define (build-azure-sdk-for-go-src)
  (use-git-fetch
   "azure-sdk-for-go-src"
   "https://github.com/curoverse/azure-sdk-for-go.git"
   "1620af6b32398bfc91827ceae54a8cc1f55df04d"
   "08fg7wyp1nlz2cfjx8p4phs586fxxabji5r082g5ixxsrq52b600"))

(define (build-mux-src)
  (use-url-fetch
   "mux-src"
   "https://github.com/gorilla/mux/archive/v1.3.0.tar.gz"
   "0k6kgz9qgl6c1zd271hnzp96yh2p2v0410k4dq0y1j3qg2qfd7m1"))

(define (build-logrus-src)
  (use-url-fetch
   "logrus-src"
   "https://github.com/sirupsen/logrus/archive/v0.11.5.tar.gz"
   "0skwgvd2d36y9fipjpbkzin9l9pnl2gk1i3xsf27497db5g5mgyj"))

(define (build-check.v1-src)
  (use-git-fetch
   "check.v1-src"
   "https://gopkg.in/check.v1"
   "20d25e2804050c1cd24a7eea1e7a6447dd0e74ec"
   "0k1m83ji9l1a7ng8a7v40psbymxasmssbrrhpdv2wl4rhs0nc3np"))

(define arvados-minimal
  (package
   (name "arvados-keepstore")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/curoverse/arvados.git")
       (commit "3b4325c210516d1f61838fb26e06e0a11c31ce6d")))
     (sha256
      (base32
       "10jz0j9gy521k1cffchwgvw0gjwhbi06cyj6bqwry7h9ybjyf8xi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f))
   (home-page "https://arvados.org/")
   (synopsis "It is a platform for data science with very large data sets")
   (description "The Arvados core is a platform for production data science with
very large data sets.  It is made up of two major systems and a number of
related services and components including APIs, SDKs, and visual tools.  The two
major systems are:
@enumerate
@item Keep: Keep is a content-addressable storage system for managing and
storing large collections of files with durable, cryptographically verifiable
references and high-throughput processing.
@item Crunch: Crunch is a containerized workflow engine for running complex,
multi-part pipelines or workflows in a way that is flexible, scalable, and
supports versioning, reproducibilty, and provenance.
@end enumerate")
   (license l:asl2.0)))

(define-public arvados-keepstore
  (package
   (inherit arvados-minimal)
   (name "arvados-keepstore")
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases
       %standard-phases
       ,(build-setup-go-workspace
	 `'("/git.curoverse.com"
	    "/github.com/AdRoll/goamz"
	    "/github.com/coreos/go-systemd"
	    "/github.com/curoverse/azure-sdk-for-go"
	    "/github.com/ghodss/yaml"
	    "/github.com/gorilla/mux"
	    "/github.com/Sirupsen/logrus"
	    "/gopkg.in/yaml.v2"
	    "/gopkg.in/check.v1"))
       (add-after 'unpack 'unpack-dependencies
		  (lambda* (#:key inputs #:allow-other-keys)
		    (let ((unpack
			   (lambda (source target)
			     (with-directory-excursion
			      target
			      (zero? (system* "tar" "xvf"
					      (assoc-ref inputs source)
					      "--strip-components=1")))))
			  (cp-src (lambda (source target)
				    (copy-recursively
				     (assoc-ref inputs source) target)))
			  (src_dir (string-append (getcwd) "/../gopath/src")))
		      (and (unpack "go-systemd-src"
				   (string-append
				    src_dir
				    "/github.com/coreos/go-systemd"))
			   (unpack "yaml-src"
				   (string-append
				    src_dir "/github.com/ghodss/yaml"))
			   (unpack "mux-src"
				   (string-append
				    src_dir "/github.com/gorilla/mux"))
			   (unpack "logrus-src"
				   (string-append
				    src_dir
				    "/github.com/Sirupsen/logrus"))
			   (cp-src "goamz-src"
				   (string-append
				    src_dir "/github.com/AdRoll/goamz"))
			   (cp-src "yaml.v2-src"
				   (string-append src_dir "/gopkg.in/yaml.v2"))
			   (cp-src "check.v1-src"
				   (string-append src_dir "/gopkg.in/check.v1"))
			   (cp-src
			    "azure-sdk-for-go-src"
			    (string-append
			     src_dir
			     "/github.com/curoverse/azure-sdk-for-go"))))))
       (delete 'configure)
       (add-before 'build 'copy-source
		   (lambda* _
		     (let ((cwd (getcwd)))
		       (copy-recursively
			cwd
			(string-append
			 cwd
			 "/../gopath/src/git.curoverse.com/arvados.git")))))
       (replace 'build
		(lambda* (#:key outputs #:allow-other-keys)
		  (let* ((gopath (string-append (getcwd) "/../gopath"))
			 (src-dir (string-append gopath "/src"))
			 (keep-src (string-append "git.curoverse.com"
						  "/arvados.git/services"
						  "/keepstore")))
		    (setenv "GOPATH" gopath)
		    (chdir (string-append gopath "/src"))
		    (system* "go" "install" keep-src))))
       (replace 'check
		(lambda* (#:key tests? #:allow-other-keys)
		  (if tests?
		      (let* ((cwd (getcwd))
			     (test-dir (string-append cwd
						      "/git.curoverse.com"
						      "/arvados.git/services"
						      "/keepstore")))
			(chdir test-dir)
			(system* "go" "test")))))
       (replace 'install
		(lambda* (#:key outputs #:allow-other-keys)
		  (let ((out (assoc-ref outputs "out"))
			(gopath (string-append (getcwd)
					       "/..")))
		    (chdir gopath)
		    (copy-recursively (string-append gopath "/bin")
				      (string-append out "/bin"))))))))
   (propagated-inputs
    `(("go" ,go)))
   (native-inputs
    `(("python-wrapper" ,python-wrapper)))
   (inputs
    `(,(build-goamz-src)
      ,(build-go-systemd-src)
      ,(build-azure-sdk-for-go-src)
      ,(build-yaml-src)
      ,(build-mux-src)
      ,(build-logrus-src)
      ,(build-yaml.v2-src)
      ,(build-check.v1-src)))))

(define-public arvados-keep-balance
  (package
   (inherit arvados-minimal)
   (name "arvados-keep-balance")
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases
       %standard-phases
       ,(build-setup-go-workspace `'("/github.com/ghodss/yaml"
				     "/gopkg.in/yaml.v2"))
       (add-after
       	'unpack
       	'unpack-dependencies
	(lambda* (#:key inputs #:allow-other-keys)
	  (let ((unpack (lambda (source target)
			  (with-directory-excursion
			   target
			   (zero? (system* "tar" "xvf"
					   (assoc-ref inputs source)
					   "--strip-components=1")))))
		(cp-src (lambda (source target)
			  (copy-recursively (assoc-ref inputs source) target)))
		(src_dir (string-append (getcwd) "/../gopath/src")))
	    (and (unpack "yaml-src"
			 (string-append src_dir "/github.com/ghodss/yaml"))
		 (cp-src "yaml.v2-src"
			 (string-append src_dir "/gopkg.in/yaml.v2"))))))
       (delete 'configure)
       (add-before 'build 'copy-source
		   (lambda* _
		     (let ((cwd (getcwd)))
		       (copy-recursively
			cwd
			(string-append
			 cwd
			 "/../gopath/src/git.curoverse.com/arvados.git")))))
       (replace 'build
		(lambda* (#:key outputs #:allow-other-keys)
		  (let* ((gopath (string-append (getcwd) "/../gopath"))
			 (src-dir (string-append gopath "/src"))
			 (keep-src (string-append "git.curoverse.com"
						  "/arvados.git/services"
						  "/keep-balance")))
		    (setenv "GOPATH" gopath)
		    (chdir (string-append gopath "/src"))
		    (system* "go" "install" keep-src))))
       (delete 'check)
       (replace 'install
		(lambda* (#:key outputs #:allow-other-keys)
		  (display (getcwd))
		  (newline)
		  (let ((out (assoc-ref outputs "out"))
			(gopath (string-append (getcwd)
					       "/..")))
		    (chdir gopath)
		    (copy-recursively (string-append gopath "/bin")
				      (string-append out "/bin"))))))))
   (propagated-inputs
    `(("go" ,go)))
   (inputs
    `(,(build-yaml-src)
      ,(build-yaml.v2-src)))))

(define-public ruby-arvados
  (package
   (name "ruby-arvados")
   (version "0.1.20170629115132")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "arvados" version))
     (sha256
      (base32
       "10zjq51r6fd73kdp02yk7fgfv9gz291gx5m2h6ah4wvr7aaxvd3s"))))
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport-4.2.9)
      ("ruby-andand" ,ruby-andand)
      ("ruby-google-api-client" ,ruby-google-api-client-0.8.7)
      ("ruby-i18n" ,ruby-i18n)
      ("ruby-json" ,ruby-json)
      ("ruby-jwt" ,ruby-jwt)))
   (arguments
    `(#:tests? #f ;; No Rakefile found
      #:phases
      (modify-phases
       %standard-phases
       (add-before
	'build
	'patch-gemspec
	(lambda* _
	  (substitute* ".gemspec"
		       (("< 4.2.6") ">= 4.2.6")))))))
   (synopsis
    "Arvados client library, git commit 060d38d627bd1e51dd2b3c6e7de9af6aa7d7b6f3")
   (description
    "Arvados client library, git commit 060d38d627bd1e51dd2b3c6e7de9af6aa7d7b6f3")
   (home-page "https://arvados.org")
   (license l:asl2.0)))

(define-public arvados-cli
  (package
   (inherit arvados-minimal)
   (name "arvados-cli")
   (build-system ruby-build-system)
   (propagated-inputs
    `(("ruby" ,ruby)
      ("ruby-arvados" ,ruby-arvados)
      ("ruby-google-api-client" ,ruby-google-api-client-0.8.7)
      ;;("ruby-activesupport" ,ruby-activesupport)
      ("ruby-trollop" ,ruby-trollop)
      ("ruby-andand" ,ruby-andand)
      ("ruby-oj" ,ruby-oj)
      ("ruby-curb" ,ruby-curb)
      ("ruby-json" ,ruby-json)))
   (native-inputs
    `(("git" ,git)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases
       %standard-phases
       (add-before
	'patch-usr-bin-file
	'change-to-cli-directory
	(lambda* _
	  (chdir (string-append (getcwd) "/sdk/cli/"))))
       (add-before
	'build
	'patch-gemspec
	(lambda* _
	  (substitute* "arvados-cli.gemspec"
		       (("/usr/bin/git") (which "git"))
		       (("~> 0.6") ">= 0.6")
		       ;;(("< 5") ">= 5")
		       (("~> 2.0") ">= 2.0")))))))))
