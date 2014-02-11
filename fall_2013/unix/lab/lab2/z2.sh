#!/bin/bash

if [[ (( $# > 0 )) && "$1"=="-d" ]]
then
    arg="%D %T"
else
    arg="%T"
fi

while true
do
    date +"$arg"
    sleep 1
    tput cuu1
done
