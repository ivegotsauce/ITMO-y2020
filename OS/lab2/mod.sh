#!/bin/bash

users=$(ps -u | awk '{print $1}' | tail -n +2 | sort | uniq)
for u in $users
do
	printf "%s " "$u"
	ps -u | awk -v var="$u" '$1==var { printf "%s ", $2 }'
	echo
done
