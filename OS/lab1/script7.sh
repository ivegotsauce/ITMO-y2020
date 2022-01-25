#!/bin/bash

REGEXP='[a-zA-Z0-9_.]+@[a-zA-Z0-9_]+(\.[a-zA-Z]+)+'
grep -r -h -o -E $REGEXP  /etc | grep -E $REGEXP | awk '{printf $1", "}' | rev | cut -c 3- | rev > emails.lst
