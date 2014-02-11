#!/bin/bash

cat <<EOF >>/etc/profile

if [ "\$(id -u)" = "0" ]; then
    echo "Welcome home, Master!"
else
    echo "Hi, \$USER!"
fi
 
if  [ ! "\$BASH" -o "\$BASH" = "/bin/sh" ]; then
    echo "All hope abandon, ye who enter in."
fi
EOF
