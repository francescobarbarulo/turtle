#!/bin/bash

TEMPDIR=/tmp/turtle
ERLDIR=erlang/src/dsmt/turtle/erlang/

mkdir -p $TEMPDIR

cp -rp erlang/junit/* $TEMPDIR

(cd $ERLDIR && erlc master_node.erl)
(cd $ERLDIR && erl -noshell -eval "master_node:start()." -sname master_node@master -setcookie turtle)


