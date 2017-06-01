# Params

if [ ! $# -eq 1 ]; then
  echo Usage: `basename $0` 'INPUT'
  echo
  exit
fi

input=$1

# Request

wget --post-data "$1" 'localhost:9000/?properties={"annotators":"tokenize,ssplit,pos,parse","parse.model":"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz","pos.model":"edu/stanford/nlp/models/pos-tagger/french/french.tagger","tokenize.language":"fr","outputFormat":"json"}' -O -
