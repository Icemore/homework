#!/bin/bash

printf "%4s" ""
for i in $( seq $1 )
do
    printf "%8d " $i
done
printf "\n"

for i in $( seq $1 )
do
    printf "%4d" $i

    for j in $( seq $1 )
    do
        printf "%8s " `echo "obase=2;$(( $i * $j ))" | bc` 
    done

    printf "\n"
done
