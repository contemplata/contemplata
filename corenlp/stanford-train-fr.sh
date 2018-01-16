# Config

memory=4g

# Params

if [ ! $# -eq 4 ]; then
  echo Usage: `basename $0` 'CORE-NLP' 'TRAIN' 'TEST' 'OUT'
  echo
  echo TRAIN: a file with syntax trees in the PTB format used for training
  echo TEST: a file with syntax trees in the PTB format used for testing 
  echo OUT: output model
  echo
  exit
fi

scriptdir=$1
trainPath=$2
testPath=$3
outModel=$4


# Training

java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.parser.lexparser.LexicalizedParser \
  -tLPP edu.stanford.nlp.parser.lexparser.FrenchTreebankParserParams \
  -uwm 1 \
  -encoding utf-8 \
  -train "$trainPath" \
  -testTreebank "$testPath" \
  -saveToSerializedFile "$outModel"
