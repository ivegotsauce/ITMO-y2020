#!/bin/bash
A=1
MODE="add"
(tail -f pipe) |
while true; do
	read LINE;
	case $LINE in
		"+")
			MODE="add"
			;;
		"*")
			MODE="mul"
			;;
		"QUIT")
			echo "exit"
			tmp=$(ps ax -o ppid,pid,comm | awk -v var=$$ 'var==$1 && $3=="tail" { print $2 }')
			kill $tmp
			exit
			;;
		*)
			if [[ $LINE =~ [0-9]+ ]]
			then
				case $MODE in
					"add")
						A=$(($A + $LINE))
						echo $A
						;;
					"mul")
						A=$(($A * $LINE))
						echo $A
						;;
				esac
				continue
			fi
			echo "Stopped by the incorrect input"
			tmp=$(ps ax -o ppid,pid,comm | awk -v var=$$ 'var==$1 && $3=="tail" { print $2 }')
			kill $tmp
			exit
			;;
	esac
done
