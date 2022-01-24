#!/bin/bash
lastBackup=$(ls /home/user | grep ^Backup- | sort -n | tail -1)
[ -z lastBackup ] && echo "no Backup" && exit
[ ! -d "/home/user/restore" ] && mkdir /home/user/restore
files=$(ls /home/user/$lastBackup)
for i in $files
do
	if [[ ! $i =~ \.[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]
	then
	cp /home/user/$lastBackup/$i /home/user/restore
	fi
done
