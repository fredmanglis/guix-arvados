#GNU Guix With Arvados

## Introduction

This repository is a sort of [GNU Guix](https://www.gnu.org/software/guix/) channel for [Arvados](https://arvados.org/)

## Install Arvados on GNU Guix

1. Clone this repository `git clone https://github.com/fredmanglis/guix-arvados.git`
2. Set GUIX_PACKAGE_PATH to the path where the repository is: example, if you've cloned the repository in `$HOME/repositories` then do something like, `cd $HOME/repositories/guix-arvados && export GUIX_PACKAGE_PATH=$(pwd)`
3. Install `guix package --install=arvados-keep`
4. Now test that it was installed successfully: `keepstore --help`
5. Configure and run keepstore as a service: `keepstore -listen=<port> -enforce-permissions=<true|false> -blob-signing-key-file=</path/to/blob-signing-key> -data-manager-token-file=</path/to/data-manager-token> -max-buffers=<maximum amount of RAM for data buffers> -volume=<List of storage volumes>` Please see `keepstore --help` for more information on the meanings of the arguments
