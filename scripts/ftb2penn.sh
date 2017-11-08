#!/bin/bash

# Params

PARAMS="-r -p --root=ROOT"
# PARAMS=

# Args

if [ ! $# -eq 1 ]; then
  # echo Usage: `basename $0` 'INPUT-DIR' 'OUTPUT-FILE'
  echo Usage: `basename $0` 'INPUT-DIR'
  exit
fi

input=$1
# output=$2

for filename in "$input"/*.xml; do
    # echo "$filename"
    # odil ftb2penn -f "$filename" >> "$output"
    odil ftb2penn -f "$filename" $PARAMS
done
