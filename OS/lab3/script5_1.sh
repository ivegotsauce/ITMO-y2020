#!/bin/bash
while true; do
	read LINE
	echo "$LINE" > pipe

	if [[ "$LINE" == "QUIT" ]]
	then
		exit
	fi

	if [[ ! "$LINE" =~ [0-9]+ && "$LINE" != "+" && "$LINE" != "*" ]]
	then
		echo "Stopped by the incorrect input"
		exit
	fi
done
