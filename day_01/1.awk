#!/usr/bin/awk
# Usage: . ./1.awk < input

awk '{ sum+=$1 } END { print sum }'
