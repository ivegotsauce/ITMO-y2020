#!/bin/bash
currentDate=$(date +%Y-%m-%d)
currentSeconds=$(date +%s)
lastDate=$(ls /home/user | grep -E ^Backup- | sort -n | tail -1 | cut -d - -f 2,3,4)
lastSeconds=$(date -d "$lastDate" +%s)
[ -z $lastDate ] && lastSeconds=0
days=$(((currentSeconds-lastSeconds)/60/60/24))
files=$(ls /home/user/source)
if [[ $days -gt 7 ]]
then
	dir="/home/user/Backup-$currentDate"
	mkdir $dir
	for i in $files
	do
	cp /home/user/source/$i $dir
	done
	printf "Directory $dir was created. Files:\n$files\n" >> /home/user/backup-report
else
	lastDirectory="/home/user/Backup-$lastDate"
	echo "Directory $lastDirectory was changed in $currentDate. Files:" >> /home/user/backup-report
	for i in $files
	do
	if [ -f $lastDirectory/$i ]
	then
		currentSize=$(stat --printf="%s" "/home/user/source/$i")
		lastSize=$(stat --printf="%s" "$lastDirectory/$i")
		if [[ $currentSize -ne $lastSize ]]
		then
			mv "$lastDirectory/$i" "$lastDirectory/$i.$currentDate"
			cp /home/user/source/$i $lastDirectory
			line=$(printf "%s~%s.%s" "$i" "$i" "$currentDate")
			echo $line >> .renamed_files
		fi
	else
		cp /home/user/source/$i $lastDirectory
		echo $i >> /home/user/backup-report
	fi
	done
	if [[ -f .renamed_files ]]
	then
	for i in $(cat .renamed_files)
	do
	echo $i | sed 's/~/ /' >> /home/user/backup-report
	done
	rm .renamed_files
	fi
fi
