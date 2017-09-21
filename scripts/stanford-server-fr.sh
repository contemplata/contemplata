# Config

# scriptdir=/home/kuba/work/odil/third-party/stanford/tool/stanford-corenlp-full-2016-10-31
scriptdir=/home/kuba/work/odil/third-party/stanford/github/CoreNLPData
memory=4g


# Parsing

# Run the server using all jars in the current directory (e.g., the CoreNLP home directory)
java -mx"$memory" -cp "$scriptdir/*:" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 9000 -timeout 15000
