;;; GNU Guix --- Functional Package management for GNU
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>

(define-module (gn packages arvados)
  #:use-module (gn packages python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages))

(define-public arvados
  (package
   (name "arvados")
   (version "0.0.0")
   (source
    (origin
     (method git-fetch)
     (uri "https://github.com/curoverse/arvados.git")
     (sha256
      (base32
       "0ysckdi9dbg1i66fxs0cl09gbx97wz7y2ml69wyraij6vjl44s48"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("go" ,go)
      ("ruby" ,ruby)
      ("bundler" ,bundler)))
   (native-inputs
    `(("bison" ,bison)
      ("fuse" , fuse)
      ("gettext" ,gettext)
      ("git" ,git)
      ("gitolite" ,gitolite)
      ("graphviz" ,graphviz)
      ("python-linkchecker" ,python-linkchecker)
      ("lsof" ,lsof)
      ("nginx" ,nginx)
      ("postgresql" ,postgresql)
      ("pkg-config" ,pkg-config)
      ("sudo" ,sudo)
      ("python-virtualenv" ,python-virtualenv)
      ("wget" ,wget)))
   (home-page "https://arvados.org/")
   (synopsis "It is a platform for data science with very large data sets")
   (description "The Arvados core is a platform for production data science with very
large data sets.  It is made up of two major systems and a number of related services and
components including APIs, SDKs, and visual tools.  The two major systems are:
@enumerate
@item Keep: Keep is a content-addressable storage system for managing and storing large
collections of files with durable, cryptographically verifiable references and
high-throughput processing.
@item Crunch: Crunch is a containerized workflow engine for running complex, multi-part
pipelines or workflows in a way that is flexible, scalable, and supports versioning,
reproducibilty, and provenance.
@end enumerate")
   (license l:asl2.0)))
