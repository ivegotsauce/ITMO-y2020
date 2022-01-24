#!/bin/bash

ps a -u user -o pid,comm | sed '1d' | wc -l > ans1.log
ps a -u user -o pid,comm | sed '1d' | awk '{print $1":"$2}' >> ans1.log
