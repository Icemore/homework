#!/bin/bash

printf "%4s" ""
for i in $( seq $1 )
do
    printf "%3d " $i
done
printf "\n"

for i in $( seq $1 )
do
    printf "%4d" $i

    for j in $( seq $1 )
    do
        printf "%3d " $(( $i * $j )) 
    done

    printf "\n"
done
