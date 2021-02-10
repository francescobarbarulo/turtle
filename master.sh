#!/bin/bash

ERLDIR=erlang/src/dsmt/turtle/erlang/

(cd $ERLDIR && erlc master_node.erl)
(cd $ERLDIR && erlc test_runner.erl)
(cd $ERLDIR && erl -noshell -eval "master_node:start()." -sname master_node@master -setcookie turtle)


