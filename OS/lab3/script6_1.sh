#!/bin/bash
while true; do
	read LINE
	case $LINE in
		"+")
			kill -USR1 $(cat .pid)
				;;
		"*")
			kill -USR2 $(cat .pid)
				;;
		"SET")
			kill -SIGSEGV $(cat .pid)
				;;
		"+3")
			kill -SIGPIPE $(cat .pid)
				;;
		"TERM")
			kill -SIGTERM $(cat .pid)
			exit
				;;
		*)
			:
			;;
	esac
done
