#!/usr/bin/awk
# Usage: . ./run.awk < input

awk '{ sum+=$1 } END { print sum }'
