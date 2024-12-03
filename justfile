set dotenv-load := true

export GUILE_REPL_SOCKET := "guile-repl.socket"

test $filter="":
    guile tests/main.scm --filter "$filter"

repl:
    rm -f $GUILE_REPL_SOCKET
    guile -L src --listen="$(pwd)/$GUILE_REPL_SOCKET" 
    rm $GUILE_REPL_SOCKET

# TODO: figure out formatting files and checking if files are formatted
# See here: https://gist.github.com/shegeley/cb44e156c10b8f235d8abd0dd768cff4
# See also: https://www.gnu.org/software/guile/manual/html_node/Pretty-Printing.html
