#!/bin/bash

ps -A -o pid,start | sed 's/://' | sed 's/://' | sort -nk2 | tail -1 | awk '{print $1}'
