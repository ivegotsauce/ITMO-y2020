#!/bin/bash

tmp=$( for pid in $(ls /proc | grep [0-9])
do
if [[ -f "/proc/$pid/io" ]]
then
bytes=$(grep "read_bytes" /proc/$pid/io | awk '{printf $2}')
fi
printf "%s %s\n" "$pid" "$bytes"
done )
sleep 60
tmp2=$( echo "$tmp" | while read line;
do
pid=$(echo $line | awk '{printf $1}')
b=$(echo $line | awk '{printf $2}')
if [[ -f "/proc/$pid/io" ]]
then
bytes=$(grep "read_bytes" /proc/$pid/io | awk '{printf $2}')
ans=$((bytes-b))
fi
if [[ -f "/proc/$pid/cmdline" ]]
then
cmd=$(cat /proc/$pid/cmdline | tr '\0' ']')
fi
if [[ !( -z $cmd ) ]]
then
printf "%s %s %s\n" "$pid" "$cmd" "$ans"
fi
done | sort -nk3 | tail -3 )
echo "$tmp2" | while read line;
do
pid=$(echo $line | awk '{printf $1}')
cmd=$(echo $line | awk '{printf $2}' | tr ']' ' ')
bytes=$(echo $line | awk '{printf $3}')
printf "%s:%s:%s\n" "$pid" "$cmd" "$bytes"
done > ans7.log


