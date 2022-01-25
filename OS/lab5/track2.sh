#!/bin/bash

printf "" > track2.log

while true; do
	sleep 1
	top -b -n 1 | tac | tail -5 | tac | tail -2 >> track2.log
	top -b -n 1 -p $1 | tail -1 >> track2.log
	top -b -n 1 | tac | tail -12 | tac | tail -5 >> track2.log
	echo >> track2.log
done
