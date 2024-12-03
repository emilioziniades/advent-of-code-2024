set dotenv-load := true

test $filter="":
    guile tests/main.scm --filter "$filter"

repl $repl_socket="guile-repl.socket":
    rm -f $repl_socket
    guile -L src -L tests --listen="$(pwd)/$repl_socket" 
    rm $repl_socket

# TODO: figure out formatting files and checking if files are formatted
# See here: https://gist.github.com/shegeley/cb44e156c10b8f235d8abd0dd768cff4
# See also: https://www.gnu.org/software/guile/manual/html_node/Pretty-Printing.html
# See also: https://github.com/lispunion/code-formatter
