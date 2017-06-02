# Params

if [ ! $# -eq 1 ]; then
  echo Usage: `basename $0` 'INPUT'
  echo
  exit
fi

input=$1

# Properties

# props=

# Request

# wget --post-data "$1" 'localhost:9000/?properties={"annotators":"tokenize,ssplit,pos,parse","parse.model":"edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz","pos.model":"edu/stanford/nlp/models/pos-tagger/french/french.tagger","tokenize.language":"fr","outputFormat":"json"}' -O -

curl --data "$1" 'localhost:9000/?properties=%7B%22annotators%22%3A%22tokenize%2Cssplit%2Cpos%2Cparse%22%2C%22parse.model%22%3A%22edu%2Fstanford%2Fnlp%2Fmodels%2Flexparser%2FfrenchFactored.ser.gz%22%2C%22pos.model%22%3A%22edu%2Fstanford%2Fnlp%2Fmodels%2Fpos-tagger%2Ffrench%2Ffrench.tagger%22%2C%22tokenize.language%22%3A%22fr%22%2C%22outputFormat%22%3A%22json%22%7D' -o -
