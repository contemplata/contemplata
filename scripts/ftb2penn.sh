#!/bin/bash

# Params

if [ ! $# -eq 2 ]; then
  echo Usage: `basename $0` 'INPUT-DIR' 'OUTPUT-FILE'
  exit
fi

input=$1
output=$2

for filename in "$input"/*.xml; do
    echo "$filename"
    odil ftb2penn -f "$filename" >> "$output"
done
