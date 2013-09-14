#!/bin/bash

ps -p $$
exit 13

shellName=`ps -p $$ h | grep -o --regex "-.*$"`
echo $shellName

