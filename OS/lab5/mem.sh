#!/bin/bash

cnt=0
arr=()
printf "" > report.log
( bash track.bash $$ ) &
echo $!
while true; do
	arr+=(1 2 3 4 5 6 7 8 9 0)
	cnt=$((cnt+1))
	[ $(( $cnt % 10000 )) = 0 ] && echo "${#arr[@]}" >> report.log
done
