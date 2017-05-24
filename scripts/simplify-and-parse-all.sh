#!/bin/bash

# Config

scriptdir=/home/kuba/work/odil/bitbucket/scripts

# Params

if [ ! $# -eq 4 ]; then
  echo Usage: `basename $0` 'INPUT-DIR' 'SIMPLIFY-DIR' 'PENN-DIR' 'JSON-DIR'
  echo
  echo Take each XML file from the INPUT-DIR, simplify it, parse it
  echo using the Stanford parser, while storing the results of parsing
  echo in PENN-DIR, and, finally, convert to JSON files which will be stored
  echo JSON-DIR.
  echo 
  echo The provisional simplification results will be stored in SIMPLIFY-DIR.
  echo
  exit
fi

input=$1
simpdir=$2
penndir=$3
output=$4

for filename in "$input"/*.xml; do

    echo === SIMPLIFYING "$filename" ===
    simple="$simpdir"/$(basename "$filename" .xml).txt
    odil simplify -a "$filename" > "$simple"
    
    echo === PREPROCESSING "$simple" ===
    prep="$simpdir"/$(basename "$filename" .xml)-prep.txt
    odil preprocess < "$simple" > "$prep"

    echo === STANFORD PARSING "$prep" ===
    penn="$penndir"/$(basename "$filename" .xml).penn
    "$scriptdir"/stanford-fr.sh "$prep" "$penn"

    echo === CONVERTING "$penn" ===
    out="$output"/$(basename "$filename" .xml).json
    odil penn2odil -o "$simple" -p "$penn" > "$out"

    echo === RESULT IN "$out" ===
done
