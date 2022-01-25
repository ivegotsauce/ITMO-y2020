#!/bin/bash
filename=$1
[ ! $# -eq 1 ] && echo "Wrong number of arguments" && exit
[ ! -f "$1" ] && echo "No such file" && exit
[[ "$filename" =~ ^[[:space:]]+ ]] && echo "Invalid file name"  && exit
[[ "$filename" =~ /+ ]] && echo "File must be located in the current directory" && exit
[ ! -d /home/user/.trash ] && mkdir /home/user/.trash
cnt="$(cat /home/user/lab4/cnt)"
ln -P "$filename" /home/user/.trash/$cnt
rm "$filename"
echo "$PWD/$filename:$cnt" >> /home/user/.trash.log
echo $((cnt+1)) > /home/user/lab4/cnt
