#!/bin/bash

for a in $( seq $1 ) 
do
    curf=input$a
    mkdir -p $curf
    
    rm -f $curf/input.txt

    for num in $( seq $2 )
    do
        echo $RANDOM >> $curf/input.txt
    done
done

