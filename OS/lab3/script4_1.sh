#!/bin/bash

bash /home/user/lab3/script4_2.sh &
pid1=$!
bash /home/user/lab3/script4_2.sh &
pid2=$!
bash /home/user/lab3/script4_2.sh &
pid3=$!

echo $pid1
echo $pid2
echo $pid3
