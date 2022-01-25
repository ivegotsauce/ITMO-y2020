#!/bin/bash

PS3='Please enter your choice: '
options=("Open nano" "Open vim" "Open links" "Quit")
select opt in  "${options[@]}"
do
case $opt in
"Open nano")
nano
;;
"Open vim")
vi
;;
"Open links")
links
;;
"Quit")
break
;;
*) echo "invalid option $REPLY"
;;
esac
done
