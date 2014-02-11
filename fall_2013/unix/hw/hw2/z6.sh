#!/bin/bash

for file in "$@"
do
    echo "$file:"
    
    ext="${file##*.}"

    if [[ "$ext" == "zip" ]]; then
        unzip -l "$file"
    else
        tar -tvf "$file"
    fi

    echo ""
done
