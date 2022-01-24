#!/bin/bash

grep -E "\(WW\)" /var/log/anaconda/X.log | sed 's/(WW)/Warning/' > full.log
grep -E "\(II\)" /var/log/anaconda/X.log | sed 's/(II)/Information/' >> full.log
cat full.log
