# Wzsh - Wez's Shell

A unixy interactive shell for Posix and Windows systems

[![Build Status](https://travis-ci.org/wez/wzsh.svg?branch=master)](https://travis-ci.org/wez/wzsh)

## Goals

* Be a convenient interactive shell
* Feel familiar to long-time unix users by using the Bourne syntax
* Have discoverable builtins and help
* Run on Windows without requiring cygwin, msys or wsl

## Non-Goals

* I don't want to replace `/bin/sh` or `/bin/bash` shebang usage.
  I don't believe in long shell scripts and I don't think wzsh
  should try to compete in that space.
