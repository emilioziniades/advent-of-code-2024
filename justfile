set dotenv-load := true

export GUILE_REPL_SOCKET := "guile-repl.socket"

test filter="":
    guile tests/main.scm {{ if filter != "" { "--filter " + filter } else { "" } }}
    cat advent-of-code.log
    rm advent-of-code.log

repl:
    rm -f $GUILE_REPL_SOCKET
    guile -L src --listen="$(pwd)/$GUILE_REPL_SOCKET" 
    rm $GUILE_REPL_SOCKET
