#!/bin/bash

for ((i=0; i < $1; i++))
do
sleep 1
( bash newmem.bash $2 ) &
done
