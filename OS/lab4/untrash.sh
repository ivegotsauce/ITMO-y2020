#!/bin/bash
filename=$1
[[ $filename =~ ^[[:space:]]+ ]] && echo "Invalid name of file" && exit
[ ! $# -eq 1 ] && echo "Wrong number of arguments" && exit
files="$(cat /home/user/.trash.log)"
trash="/home/user/.trash"
log="/home/user/.trash.log"
echo "" > /home/user/.trash.log
for i in $files
do
	if [[ "$i" =~ "$filename:"[0-9]+$ ]]
	then
		filePath="$( echo $i | cut -d : -f 1)"
		fileDirectory="$( echo $i | cut -d : -f 1 | grep -oP ".*/")"
		fileName="$(basename $i | cut -d : -f 1)"
		echo "Do you want to restore $filePath? y\n"
		read response
		if [ $response = "y" ]
		then
		if [ ! -d $fileDirectory ]
		then
			echo "File directory doesn't exist. Do you want to place it in a new directory? y\n"
			read responseDirectory
			if [ $responseDirectory = "y" ]
			then
				while true; do
				echo "Write new directory"
				read directory
				if [ -d $directory ]
				then
					filePath="$directory/$fileName"
					fileDirectory="$directory/"
					echo "File will be restored in $directory"
					break
				else
					echo "Directory doesn't exist. Try again."
				fi
				done
			else
				echo "$i" >> "$log"
				exit
			fi
		fi
		fileId="$( echo $i | cut -d : -f 2 )"
		fileInTrash="$trash/$fileId"
			if [ ! -f "$filePath" ]
			then
			ln -P "$fileInTrash" "$filePath"
			rm "$fileInTrash"
				else
				echo "File already exists. Do you want to rename it? y\n"
				read responseRename
				if [ $responseRename = "y" ]
				then
					echo "Write new name of file"
					while true; do
					read name
					if [ ! -f "$fileDirectory$name" ]
					then
					ln -P "$fileInTrash" "$fileDirectory$name"
					rm "$fileInTrash"
					break
						else
						echo "$fileDirectory$name already exists. Try again."
					fi
					done
					else
					echo "$i" >> "$log"
				fi
			fi
		else echo "$i" >> "$log"
		fi
	else echo "$i" >> "$log"
	fi
done
