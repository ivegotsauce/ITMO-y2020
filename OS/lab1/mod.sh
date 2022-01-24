#!/bin/bash

find -type f -exec wc -w {} \; | sort -nk 1 | awk '{print $2}' | sed 's/.\///'
