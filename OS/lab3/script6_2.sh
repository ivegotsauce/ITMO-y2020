#!/bin/bash
echo $$ > .pid
A=1
MODE="default"
usr1()
{
	MODE="add"
}
usr2()
{
	MODE="mul"
}
term()
{
	MODE="term"
}
SET()
{
	MODE="set"
}
pipe()
{
	MODE="add3"
}
trap 'usr1' USR1
trap 'usr2' USR2
trap 'term' SIGTERM
trap 'SET' SIGSEGV
trap 'pipe' SIGPIPE
while true; do
	case $MODE in
		"add")
			let A=$A+2
			echo $A
			;;
		"mul")
			let A=$A*2
			echo $A
			;;
		"term")
			echo "Stopped by SIGTERM"
			exit
			;;
		"set")
			let A=1
			echo $A
			MODE="default"
			;;
		"add3")
			let A=$A+3
			echo $A
			;;
		"default")
			:
			;;
	esac
	sleep 1
done
