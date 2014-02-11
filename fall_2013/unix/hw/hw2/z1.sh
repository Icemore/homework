#!/bin/bash

groupID=`grep -m 1 "^$1:" /etc/group | cut -d: -f 3`
cat /etc/passwd | cut -d: -f 4 | grep "^$groupID$" | wc -l
