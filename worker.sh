#!/bin/bash

TEMPDIR=/tmp/turtle
ERLDIR=erlang/src/dsmt/turtle/erlang/

PATH="$PATH:/home/giomba/Scaricati/jdk-8u152-linux-x64/jdk1.8.0_152/bin"

mkdir -p $TEMPDIR

cp -rp erlang/junit/* $TEMPDIR

(cd $ERLDIR && erlc test_runner.erl)
(cd $ERLDIR && erl -noshell -sname test_runner@master -setcookie turtle)


