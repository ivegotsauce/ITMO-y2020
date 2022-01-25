#!/bin/bash

arr=()
while true; do
	arr+=(1 2 3 4 5 6 7 8 9 0)
	[ ${#arr[@]} -gt $1 ] && break
done
