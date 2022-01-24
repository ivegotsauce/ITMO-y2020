#!/bin/bash

for i in $(ls /proc | grep [0-9])
do
if [[ -f "/proc/$i/status" ]]; then
vmsize=$(grep -i "vmsize" /proc/$i/status | awk '{printf $2}')
if [[ !( -z $vmsize ) ]]
then
printf "%s %s\n" "$i" "$vmsize"
fi
fi
done | sort -nk 2 | tail -1 | awk '{print $1}'
