#!/bin/bash

title=`date +"%Y_%m_%d_%T"`
mkdir /home/user/test && echo "catalog test was created succesfully" > ~/report &&
	touch /home/user/test/$title

ping www.net_nikogo.ru 2> /dev/null || printf "%s Ping has failed\n" $title >> ~/report

