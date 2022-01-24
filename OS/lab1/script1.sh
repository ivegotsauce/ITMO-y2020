#!/bin/bash

A=$1
if [[ $2 -gt $A ]]
then A=$2
fi
if [[ $3 -gt $A ]]
then A=$3
fi
echo $A
