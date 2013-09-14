#!/bin/bash

file="$1"
ext=`echo "${file##*.}" | rev`
name="${file%.*}"

mv "$file" "$name.$ext"
