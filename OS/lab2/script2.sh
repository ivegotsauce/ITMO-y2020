#!/bin/bash

printf "" > ans2.log
for i in $(ls /sbin/)
do
ps -A -o pid,comm | grep $i | awk '{print $1}' >> ans2.log
done
