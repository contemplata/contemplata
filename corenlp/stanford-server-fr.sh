# Config

memory=4g

# Args

if [ ! $# -eq 2 ]; then
  echo Usage: `basename $0` 'CORE-NLP' 'FR-MODELS'
  exit
fi

# scriptdir=/home/kuba/work/odil/third-party/stanford/tool/stanford-corenlp-full-2016-10-31
# scriptdir=/home/kuba/work/odil/third-party/stanford/github/CoreNLPData

# scriptdir=/home/kuba/work/github/test/stanford/stanford-corenlp-full-2017-06-09
# modelpath=/home/kuba/work/github/test/stanford/stanford-french-model.jar

scriptdir=$1
modelpath=$2

# Parsing

# Run the server using all jars in the current directory (e.g., the CoreNLP home directory)
java -mx"$memory" -cp "$scriptdir/*:$modelpath" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 9000 -timeout 15000
