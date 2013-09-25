#!/bin/bash

sed -rn 's/.*#\s*include\s*[<"](.+)\.h[>"]/\1/p' file.c
