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

The front-end is implemented in [Elm](http://elm-lang.org/), a Haskell-like
language which compiles to javascript. It allows the actual annotation of the
files in the dababase. Being a high-level language, Elm allows to implement
sophisticated annotation-related functionality relatively quickly. See **TODO**
for more information about the front-end.


# Installation
