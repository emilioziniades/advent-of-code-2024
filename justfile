set dotenv-load := true

alias t := test
alias w := watch
alias r := repl

default:
    @just --list --unsorted

test $filter="":
    guile tests/main.scm --filter "$filter"

watch $filter="":
    watchexec --clear -- just test "$filter"

repl $repl_socket="guile-repl.socket":
    rm -f $repl_socket
    guile -L src -L tests --listen="$(pwd)/$repl_socket" 
    rm $repl_socket

format:
    find . -type f -name "*.scm" -exec scheme-format -i {} \;

format-check:
    git diff --exit-code
    just format
    git diff --exit-code
