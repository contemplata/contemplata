# Config

# scriptdir=/home/kuba/work/odil/third-party/stanford/tool/stanford-parser-full-2016-10-31
scriptdir=/home/kuba/work/odil/third-party/stanford/github/CoreNLPData
memory=4000m


# Params

if [ ! $# -eq 3 ]; then
  echo Usage: `basename $0` 'TRAIN' 'TEST' 'OUT'
  echo
  echo TRAIN: a file with syntax trees in the PTB format used for training
  echo TEST: a file with syntax trees in the PTB format used for testing 
  echo OUT: output model
  echo
  exit
fi

trainPath=$1
testPath=$2
outModel=$3


# Training

# java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
#   -tLPP edu.stanford.nlp.parser.lexparser.FrenchTreebankParserParams \
#   -train "$trainPath" \
#   -testTreebank "$testPath" \

java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
  -tLPP edu.stanford.nlp.parser.lexparser.FrenchTreebankParserParams \
  -uwm 1 \
  -encoding utf-8 \
  -train "$trainPath" \
  -testTreebank "$testPath" \
  -saveToSerializedFile "$outModel"
