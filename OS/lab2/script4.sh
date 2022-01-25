#!/bin/bash

printf "" > ans4.log
for i in $(ls /proc | grep [0-9])
do
if [[ -f "/proc/$i/status" ]]; then
printf "ProcessId="$i" : Parent_ProcessID= " >> ans4.log
grep "PPid" /proc/$i/status | awk '{printf $2}' >> ans4.log
ser=$(grep "sum_exec_runtime" /proc/$i/sched | awk '{printf $3}')
ns=$(grep "nr_switches" /proc/$i/sched | awk '{printf $3}')
printf " : Average_Running_Time=" >> ans4.log
echo "scale=3; $ser/$ns" |bc >>ans4.log
fi
done
cat ans4.log | sort -nk 4 > ans4.log
grep "Parent_ProcessID= " ans4.log | sed 's/Parent_ProcessID= /Parent_ProcessID=/' > ans4.log
