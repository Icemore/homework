#!/bin/bash

declare -A arr

f(){
    for email in ${!arr[@]}
    do
        printf "%s %s\n" ${arr[$email]} $email
    done
}

for email in `grep -P -o "[a-zA-Z]+@([a-zA-Z]+\.)+[a-zA-Z]+" $1`
do
    if [ -z "${arr[$email]}" ]
    then
        arr[$email]=0
    fi

    arr[$email]=$(( ${arr[$email]} + 1))
done

f | sort -r -k2 -n | cut -d' ' -f2

