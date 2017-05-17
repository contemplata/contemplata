#!/bin/bash

# Params

if [ ! $# -eq 2 ]; then
  echo Usage: `basename $0` 'INPUT-DIR' 'OUTPUT-DIR'
  echo
  echo Convert to UTF-8 each file in INPUT-DIR
  echo and place the results in OUTPUT-DIR
  echo
  exit
fi

input=$1
output=$2

for filename in "$input"/*.xml; do
    # ./MyProgram.exe "$filename" "Logs/$(basename "$filename" .txt)_Log$i.txt"
    iconv -t UTF-8 -f ISO-8859-1 "$filename" > "$output"/$(basename "$filename")
    # echo "$filename"
    # echo $(basename "$filename")
    # echo "$output"/$(basename "$filename")
done
