#!/bin/bash

A=""
while true
do
read B
if [[ $B != "q" ]]
then A=$A" "$B
else break
fi
done
echo $A
