# Config

scriptdir=/home/kuba/work/odil/third-party/stanford/tool/stanford-parser-full-2016-10-31
memory=4000m


# Params

if [ ! $# -eq 2 ]; then
  echo Usage: `basename $0` 'MODEL' 'INPUT'
  echo
  exit
fi

modelPath=$1
inpPath=$2


# Parsing

# java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
#   -tLPP edu.stanford.nlp.parser.lexparser.FrenchTreebankParserParams \
#   -encoding utf-8 \
#   -train "$trainPath" \
#   -testTreebank "$testPath" \
#   -saveToSerializedFile "$outModel"

java -mx100m -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
   -tLPP edu.stanford.nlp.parser.lexparser.FrenchTreebankParserParams \
   -encoding utf-8 \
   "$modelPath" "$inpPath"
