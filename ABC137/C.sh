#!/bin/bash

cat - | tail -n+2 | while read line; do echo $line | grep -o . | sort | tr -d "\n" | sed 's/$/\n/'; done | sort | uniq -c | awk '{n+=($1*($1-1)/2)} END{print n}'