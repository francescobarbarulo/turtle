#!/bin/sh

for i in `seq 1 10`;
do
	echo $i:  $(./turtle.sh) &
	sleep 0.5s
done