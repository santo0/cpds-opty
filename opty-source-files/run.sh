#!/bin/bash

not_sourced() {
    >&2 echo "ERROR: Script should be sourced"
    exit 1
}

(return 0 2>/dev/null) || not_sourced

CLIENTS=30
ENTRIES=3
READS=100
WRITES=300
TIME=3
#EXECID=0


erl -eval "opty:start($CLIENTS,$ENTRIES,$READS,$WRITES,$TIME)"
#erl -eval "opty:start($CLIENTS,$ENTRIES,$READS,$WRITES,$TIME,$EXECID)"
