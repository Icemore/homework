#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Wrong number of arguments"
    exit
fi

arr[0]=0
arr[1]=1

for i in $( seq 2 $1 )
do
    arr[$i]=$(( ${arr[$(($i-1))]} + ${arr[$(($i-2))]} ))
done


for i in $( seq 0 $1 )
do
    echo ${arr[$i]}
done

