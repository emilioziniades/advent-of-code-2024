set dotenv-load := true

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
    git status --porcelain && echo "cannot check formatting with unstaged changes" && exit 1
    just format
    git status --porcelain && echo "there are unformatted files" && exit 1
