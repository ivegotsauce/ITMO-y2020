#!/bin/bash

i=0
m=0
p=0
b=0
k=$(cat ans4.log | wc -l)
c=0
cat ans4.log | while read line;
do
ppid=$(echo $line | awk '{print $3}' | sed 's/Parent_ProcessID=//')
art=$(echo $line | awk '{print $5}' | sed 's/Average_Running_Time=//')
if [[ $p -ne $ppid ]]
then
b=$(echo "scale=3; $m/$i" | bc)
i=0
m=0
printf "Average_Running_Children_of_ParentID=%s is %s\n" "$p" "$b"
p=$ppid
fi
i=$((i+1))
m=$(echo "scale=3; $m+$art" | bc)
echo $line
c=$((c+1))
if [[ c -eq k ]]
then
b=$(echo "scale=3; $m/$i" | bc)
printf "Average_Running_Children_of_ParentID=%s is %s\n" "$p" "$b"
fi
done > ans5.log

