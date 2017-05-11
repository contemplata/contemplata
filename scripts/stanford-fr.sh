# Config

scriptdir=/home/kuba/work/odil/third-party/stanford/tool/stanford-parser-full-2016-10-31
memory=4000m


# Params

if [ ! $# -eq 2 ]; then
  echo Usage: `basename $0` 'INPUT' 'OUTPUT'
  echo
  echo INPUT: one sentence per line
  echo OUTPUT: one tree per sentence
  echo
  exit
fi

input=$1
output=$2


# Parsing

java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
  -encoding UTF-8 \
  -outputFormat "penn" \
  -sentences newline \
  -writeOutputFiles \
  edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz \
  $input

mv $input.stp $output
