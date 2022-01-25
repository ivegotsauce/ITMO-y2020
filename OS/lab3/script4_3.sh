#!/bin/bash

cpulimit -p $1 -l 10 &
echo &!
