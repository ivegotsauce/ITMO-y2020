#!/bin/bash

printf "" > track.log

while true; do
	sleep 1
	top -b -n 1 | tac | tail -5 | tac | tail -2 >> track.log
	top -b -n 1 -p $1 | tail -1 >> track.log
	top -b -n 1 | tac | tail -12 | tac | tail -5 >> track.log
	echo >> track.log
done
