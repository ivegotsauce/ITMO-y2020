#!/bin/bash

if [[ $PWD == ~ ]]
then
echo ~
exit 0
else
echo "Error"
exit 1
fi
