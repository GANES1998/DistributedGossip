rm ebin/*.beam

erlc -o ebin src/*.erl

source variables.env

$ERLANG_BIN -pa "ebin" -eval "gossip_supervisor:main($1, $2, $3)." -s init stop -noshell.