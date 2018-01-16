# Training Your Own CoreNLP French Model

This README explains how to train your own CoreNLP syntactic parsing model. At
the same time, it documents the steps we took within the context of the
[Temporal@ODIL][temporal-odil] project in order to obtain a model better adapted
to spoken French data.

## French Treebank

To convert the current (as of 16/01/2018) version of the
[French Treebank](http://ftb.linguist.univ-paris-diderot.fr/) (FTB) to the PTB
format, use the following command:

```bash
odil ftb2penn -f <FTB-XML-FILE>
```

where:

* `odil` is a tool installed together with Contemplata; it can be found in
  `~/.local/bin`,
* `FTB-XML-FILE` is a file with constituency trees stored in the FTB specific
  XML format and encoded in UTF-8.

This will produce output in the PTB format, i.e., with lines similar to the one
below.

```
(SENT (ADV Ailleurs) (NP (DET l') (NC année) (NC record) (PP (P de) (NP (DET la) (NC sécheresse)))) (VN (V reste)) (ADV souvent) (NP (NC 1921)))
```
  
Run `odil ftb2penn --help` to see more conversion option.

The option we used in the Temporal@ODIL project to obtain a CoreNLP model better
adapted to spoken transcribed texts is `-r`, which simply removes punctuation
from the syntactic trees (as punctuation is not present in the trascribed ANCOR
texts).

## Training

Once you have data in the PTB format, you can use the `stanford-train-fr.sh`
script to train a Stanford lexicalised parsing model on top of it:

```bash
./stanford-train-fr.sh <stanford-corenlp> <train.ptb> <eval.ptb> <model.gz>
```

where:

* `stanford-corenlp` is the directory of the Stanford CoreNLP tool (see [this](../README.md#stanford) for more information)
* `train.ptb` is the training PTB file
* `eval.ptb` is the evaluation PTB file
* `model.gz` is the target model file (in which the output parsing model will be stored)

## Packaging

Before you can use the resulting model, `model.gz`, with Contemplata, you will
need to package it with the other French-dedicated models (tokenization, POS
tagging, etc.) provided in the official CoreNLP French model distribution.

Assuming the official CoreNLP French models are in the
`stanford-french-models.jar` file, the following sequence of commands will give
you the `new-stanford-odil-models.jar` file with the update lexicalized parsing
model, without changing the other (tokenization, POS tagging) models.

```bash
unzip stanford-french-models.jar -d stanford-french-models
cp model.gz ./stanford-french-models/edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz
cd stanford-french-models
zip -r ../new-stanford-odil-models.jar *
cd ..
rm -r stanford-french-models
```

**WARNING**: be aware that the lexicalized syntactic parser should be trained on
data consistent with the tokenizer and the POS tagger. For instance, if the POS
tagger is based on a different POS tagset than your training data, the resulting
ensemble `new-stanford-odil-models.jar` model will most likely yield very poor
performance.



[temporal-odil]: https://hal.archives-ouvertes.fr/hal-01627261 "Temporal@ODIL"
