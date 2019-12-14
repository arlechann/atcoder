#!/bin/bash

pi='3. 14159 26535 89793 23846 26433 83279 50288 41971 69399 37510 58209 74944 59230 78164 06286 20899 86280 34825 34211 70679'

read n;
if [ $n = 3 ]; then
	echo 0
else
	echo $pi | tr -d ' ' | grep -o . | tail -n+3 | grep -Fn $n | head -n1 | cut -d: -f1
fi