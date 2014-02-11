#!/bin/bash

ifconfig | sed -r "/inet addr/s/[0-9]/x/g;/Link encap/s/(.*)/`printf "%0.s-" {1..30}`\n\1/"

