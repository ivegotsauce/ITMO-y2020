#!/bin/bash

at now + 2 minutes < script1.sh &

(tail -n 0 -f ~/report) | while read line; do
echo $line
done
