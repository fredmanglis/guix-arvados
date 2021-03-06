# GNU Guix With Arvados

## Introduction

This repository is a sort of [GNU Guix](https://www.gnu.org/software/guix/) channel for [Arvados](https://arvados.org/)

## Install Arvados Components on GNU Guix

1. Clone this repository

	`git clone https://github.com/fredmanglis/guix-arvados.git`

2. Set GUIX_PACKAGE_PATH to the path where the repository is: example, if you've cloned the repository in `$HOME/repositories` then do something like, 

	`cd $HOME/repositories/guix-arvados && export GUIX_PACKAGE_PATH=$(pwd)`

### Install keepstore:

        `guix package --install=arvados-keepstore`

Now test that it was installed successfully:

	`keepstore --help`

Configure and run keepstore as a service:

	`keepstore -listen=<port> -enforce-permissions=<true|false> -blob-signing-key-file=</path/to/blob-signing-key> -data-manager-token-file=</path/to/data-manager-token> -max-buffers=<maximum-amount-of-RAM-for-data-buffers> -volume=<List-of-storage-volumes>`

Please see `keepstore --help` for more information on the meanings of the arguments

### Install keep-balance:

        `guix package --install=arvados-keep-balance`

Test that it was installed successfully:

        `keep-balance --help`

## Configuration

Provided in this repository is a file, `sample_keepstore.yml` that provides a sample configuration for keepstore that can be used to get up and running.
We recommend, however, that you run `keepstore --help` to understand what the various options in the file are, and then proceed to change them to suit your environment, and to harden security.

The BlobSigningKey needs to be long, say at least 50 characters. One way to generate such a file would be to do something like, `openssl rand -hex 50 > blob_signing_key_file`. Please see [the arvados keep server documentation](https://doc.arvados.org/install/install-keepstore.html) for more.

The SystemAuthToken could be  generated the same way as above.

A sample configuration file for keep-balance is provided. Please see `sample_keep-balance.yml` in this repository.