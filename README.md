# Contemplata

*Contemplata* is an annotation tool created with the rather specific needs of
the ODIL project in mind, namely:

* Correction of constituency trees, obtained with a syntactic parser plugged
  into Contemplata,
* Annotation of temporal entities on top of the syntactic structures,
* Linking the entities with temporal relations.

Contemplata is particularly adapted to spoken (transcribed) dialoges. In
particular, it allows to (either automatically or manually) remove selected
expressions (which is useful when these are deemed uninteresting from the
temporal annotation point of view), and to merge several speech turns into one
syntactic unit (so that it is analyzed with a single syntactic tree).


# Architecture

Contemplata is a web-based tool. Once it is installed on a server machine and
made publicly available, it can be used by the annotators without the need to
install any specific tools on their local machines (apart form a compatible
browser).

An ODIL-dedicated instance of the tool can be found at
[http://vega.info.univ-tours.fr/odil/current](http://vega.info.univ-tours.fr/odil/current).
Log in as a *guest* (password=*guest*) to have a look. As a guest, you will not
be allowed to store the changes you made, but you will have access to the
[user's guide](http://vega.info.univ-tours.fr/odil/current/user/guide) and will
be able to play with the tool's functionality.
  
On the server side, the tool uses a simple file-based storage for the annotated
files. All the files are stored in the dedicated JSON format. See **TODO** for
more information about the format.

The web-server is implemented in [Snap](http://snapframework.com/), a
[Haskell](https://www.haskell.org/) web framework. It handles regular HTTP
requests (used to list the files, general administration work, etc.) and
[WebSocket](https://en.wikipedia.org/wiki/WebSocket) requests, the latter used
to communicate with the front-end annotation application.
See **TODO** for more information about the back-end.

The front-end is implemented in [Elm][elm], a Haskell-like
language which compiles to javascript. It allows the actual annotation of the
files in the dababase. Being a high-level language, Elm allows to implement
sophisticated annotation-related functionality relatively quickly. See **TODO**
for more information about the front-end.


# Installation

First clone the Contemplata's repository into a local directory. Then proceed
with the installation of the back-end server, the front-end annotation tool, and
(optionally) the third-party syntactic analysis tools.

## Web-server

It is recommanded to install the back-end server using the
[Haskell Tool Stack][stack], which you will need to download and install on your
machine beforehand.

Then, move to the `backend` directory in the local copy of the repository and
run:

    stack install 
 
Under linux, this command will by default install the `odil-snap` command-line
tool in the `~/.local/bin` directory. You can either add this directory to your
`$PATH`, or use the full path to run `odil-snap`:

    $ ~/.local/bin/odil-snap --help
    

## Front-end

To install the front-end application, you will need to install [Elm][elm]
beforehand. Then, move to the `annotool` directory and run:

    elm-make src/Main.elm --output=main.js
    
The `--output` option tells the compiler to generate a `main.js` JavaScript file
rather than a stand-alone HTML file. You will then need to put the `main.js`
file into a directory in which the web-server is run, as explained in the
[setup](#setup) section below.


## Third-party

TODO


## Setup

You will need to prepare a dedicated enviroment to run Contemplata, i.e., a
directory where the database and all the high-level configuration files are
stored. Under linux, assuming that `$odil` is the path to the new directory, and
that `$contemplata` is the path to the local copy of the Contemplata's
repository, you can run the following commands:

    mkdir $odil
    cd $odil
    odil createdb -d DB

The last command above creates an empty database in the `DB` subdirectory. Then
you can copy the initial configuration files from the repository's `config`
subdirectory.

    cp -r $contemplata/config/* ./
    
You can read more about the configuration files in the corresponding
[README](config/README).

You also need to copy the webserver templates:

    cp -r $contemplata/backend/snaplets ./
    
To finish the setup, copy the JavaScript file generated with Elm (see the
[front-end](#front-end) section):

    cp -r $contemplata/annotool/main.js resources/public/


[this]: https://github.com/kawu/contemplata
[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[elm]: http://elm-lang.org
