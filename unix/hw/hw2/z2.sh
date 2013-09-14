#!/bin/bash


function fib {
    if (( $1 < 2 )); then
        ans[$1]=$1
    fi

    if [ -z "${ans[$1]}" ]; then

        fib $(( $1 - 1 ))
        fib $(( $1 - 2 ))

        let ans[$1]=${ans[$(( $1-1))]}+${ans[$(($1-2))]}
    fi
}

fib $1
echo ${ans[$1]}
