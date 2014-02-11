#!/bin/bash

if (( $# < 2 )); then
    echo "2 arguments required" >&2
    exit 1
fi

tr -dc A-Za-z0-9 < /dev/urandom | fold -w $2 | head -n $1
