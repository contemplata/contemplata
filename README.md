# Contemplata

*Contemplata* is an annotation tool developed specifically for the purpose of
the [Temporal@ODIL][temporal-odil] project. The ultimate goal of this project is
to annotate a portion of the [ANCOR][ancor] spoken French corpus with semantic
(more precisely, temporal) information. To this end, Contemplata allows:

* Merging/splitting speech turns into syntactically coherent units,
* Removing (either automatically or manually) selected expressions,
  uninteresting from the semantic point of view (e.g., social
  obligations-related expressions)
* Correcting constituency trees, obtained with a syntactic parser plugged into
  Contemplata,
* Annotating temporal entities on top of the syntactic structures,
* Linking the entities with temporal relations.

<!---
Contemplata is particularly adapted to spoken (transcribed) dialoges. In
particular, it allows to (either automatically or manually) remove selected
expressions (which is useful when these are deemed uninteresting from the
temporal annotation point of view), and to merge several speech turns into one
syntactic unit (so that it is analyzed with a single syntactic tree).
--->


# Installation

First clone the Contemplata's repository into a local directory.

```bash
git clone https://github.com/kawu/contemplata.git
cd contemplata
```

Then proceed with the installation of the back-end server, the front-end
annotation tool, and (optionally) the third-party syntactic analysis tools, as
explained below.

## Back-end

To install the back-end, you will need to download and install the
[Haskell Tool Stack][stack] on your machine beforehand.

Then, move to the `backend` directory and run the installation process with
`stack`.

```bash
cd backend
stack install
cd ..
```
 
Under linux, this command will (by default) install the `odil-snap` command-line
tool in the `~/.local/bin` directory. You can either add this directory to your
`$PATH`, or use the full path to run `odil-snap`:

```bash
~/.local/bin/odil-snap --help
```
    

## Front-end

To install the front-end application, you will need to install [Elm][elm]
beforehand. Then, move to the `annotool` directory and generate the JavaScript
application file.

```bash
cd annotool
elm-make src/Main.elm --output=main.js
cd ..
```
    
The `--output` option tells the compiler to generate a `main.js` JavaScript file
rather than a stand-alone HTML file. You will then need to put the `main.js`
file into a directory in which the web-server is run, as explained in the
[setup](#setup) section below.


## Third-party

You can optionally install one or both constituency parsers supported by
Contemplata. This will allow the annotators to run these parsers directly via
the annotation interface.
<!---(e.g., after changes in segmentation or POS tags).--->

### Stanford

Let `$corenlp` be the directory in which you wish to put the Stanford CoreNLP
tool. You can download the tool from the [CoreNLP's webpage][corenlp].
<!---or use the following commands (which download CoreNLP `3.8.0`, a version
against which Contemplata has been tested).--->

```bash
cd $corenlp
wget http://nlp.stanford.edu/software/stanford-corenlp-full-2017-06-09.zip
unzip stanford-corenlp-full-2017-06-09.zip
```

Next, you will need to obtain an appropriate parsing model. Currently,
Contemplata is configured to work with the French models only (we plan to allow
other languages in future versions). These models are also available at the
[CoreNLP's website][corenlp].
<!---You can download the official CoreNLP French models from the
[CoreNLP's website][corenlp], or use the following command:--->

```bash
cd $corenlp
wget http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
```

Finally, you can run the CoreNLP server, supplying it with (i) the path to the
CoreNLP's source code and (ii) the French models:

```bash
cd $contemplata/corenlp
./stanford-server-fr.sh $corenlp/stanford-corenlp-full-2017-06-09 $corenlp/stanford-french-corenlp-2017-06-09-models.jar
```

See also the [README](corenlp/README.md) file for information about the CoreNLP
French parsing model prepared within the context of the Temporal@ODIL project.

TODOs:

* Which port is used by default. By the way, it should be possible to configure
  such things.


### DiscoDOP

TODO


# Usage

Before you can start the Contemplata application, you will need to set up an
instance with its own dedicated database and configuration files.

## Setup

You will need to prepare a dedicated enviroment to run Contemplata, i.e., a
dedicated directory where the database and all the configuration files are
stored. Under linux, assuming that `$odil` is the path to the dedicated
directory, and that `$contemplata` is the path to the cloned Contemplata's
repository, you can run the following commands to create an empty database in
`$odil`'s' `DB` subdirectory.

```bash
mkdir $odil
cd $odil
odil createdb -d DB
```

Then you can copy the (a) initial configuration files, (b) webserver templates,
and (c) the JavaScript file generated with Elm (see the [front-end](#front-end)
section), using the following commands:

```bash
cp -r $contemplata/config/* ./
cp -r $contemplata/backend/snaplets ./
cp -r $contemplata/annotool/main.js resources/public/
```
    
You can read more about configuration in the corresponding
[README](config/README.md) file.

## Running

Use the following command to run the web-server in the `$contemplata` directory.

```bash
odil-snap
```

By default, the application uses the port `8000`. You can change it using the
`-p` option.

```bash
odil-snap -p 8000
```

At this point, you can access the annotation tool via
[http://localhost:8000](http://localhost:8000) (assuming that you performed the
steps described in the [setup](#setup) section).

To start annotating, you will have to log in as administrator (login = `admin`,
password = `admin`), change the password, create annotator accounts, upload
files, and assign the files to the individual annotators.


# Architecture

Contemplata is implemented in a client/server architecture, with the advantage
that the annotator does not have to install anything locally, and the server can
provide the user with more advanced functionality. For instance, the server can
be requested to syntactically re-analyze a given sentence in a way which takes
the constraints specified directly by the annotator (e.g. a particular
tokenization) into account. In the long run, the client/server architecture
should also allow a more collaborative annotation style.

On the server-side, Contemplata tool uses a simple file-based storage for the
annotated files. All the files are kept in the [dedicated JSON format](#format).

The web-server is implemented in [Snap](http://snapframework.com/), a
[Haskell](https://www.haskell.org/) web framework. It handles regular HTTP
requests (used to list the files, general administration work, etc.) as well as
[WebSocket](https://en.wikipedia.org/wiki/WebSocket) requests, the latter used
to communicate with the front-end annotation application.

The front-end is implemented in [Elm][elm], a Haskell-like language which
compiles to JavaScript, thus the tool can be used in any modern internet
browser. Being a high-level language, Elm allows to implement sophisticated
annotation-related functionality relatively quickly.

An Temporal@ODIL-dedicated instance of the tool can be found at
[http://vega.info.univ-tours.fr/odil/current](http://vega.info.univ-tours.fr/odil/current).
You can log in as a *guest* (password *guest*) to have a look. As a guest, you
will not be allowed to store any changes you made, but you will have access to
the [user's guide](http://vega.info.univ-tours.fr/odil/current/user/guide) and
will be able to play with the tool's functionality.
  

# Format

All the files in the database are stored in a dedicated JSON format. This format
is
[determined automatically](https://github.com/kawu/contemplata/blob/dev/backend/src/Odil/Server/Types.hs#L258-L282)
on the basis of the corresponding
[File](https://github.com/kawu/contemplata/blob/dev/backend/src/Odil/Server/Types.hs#L151-L187)
data type.

You can think of the File type as a definition of the structure against which
the JSON files can be validated. You can perform the validation programatically.
First run `stack ghci` within the `backend` source directory and then:

```Haskell
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
JSON.decodeStrict <$> BS.readFile "<path-to-json>" :: IO (Maybe File)
```

## French Treebank

TODO: how to obtain JSON file from a French Treebank XML file



[this]: https://github.com/kawu/contemplata
[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[elm]: http://elm-lang.org
[temporal-odil]: https://hal.archives-ouvertes.fr/hal-01627261 "Temporal@ODIL"
[ancor]: https://hal.archives-ouvertes.fr/hal-01075679 "ANCOR"
[corenlp]: https://stanfordnlp.github.io/CoreNLP/ "CoreNLP"
