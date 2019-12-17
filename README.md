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
  [More information](https://github.com/wez/wzsh/issues/2)

## Implementation Status

In no particular order, except that completed items bubble up to the top:

* [x] - Executes simple commands, pipelines, input/output redirection
* [x] - Parameter substitution ($FOO)
* [x] - Globbing and filename generation
* [x] - Basic job control (ctrl-z to background, `bg` and `fg` to manage a backgrounded job)
* [x] - Define and execute functions
* [x] - Conditionals of the form `true && echo yes` and `if`/`then`/`else`/`elif`/`fi`
* [ ] - looping constructs such as `for`, `while`, `until`
* [ ] - `case`/`esac` matching construct
* [ ] - persistent history and builtins for examining history
* [ ] - tab completion of commands, filesystem entries
* [ ] - line editor functions that can search and match history (ctrl-R!)
* [ ] - command substitution `$(date)`
