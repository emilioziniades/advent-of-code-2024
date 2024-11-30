export GUILE_REPL_SOCKET := "guile-repl.socket"

repl:
    rm -f $GUILE_REPL_SOCKET
    guile --listen="$(pwd)/$GUILE_REPL_SOCKET"
    rm $GUILE_REPL_SOCKET
