# Configuration

This directory contains several configuration files which allow to control
Contemplata's behavior.


## devel.cfg

There is a couple of top-level variables defined in the `devel.cfg` file. Below
you can see the list of the more important ones.

### DB 

Path to the database directory.

### href-base

The `href-base` variable, based on `base`, defines the base URL of the website.
The default URL address is fine for development, i.e., when you run your own
local instance with the default parameters (e.g. the port number) and want to
access the annotation tool on your local machine only.

A related variable is `websocket-server`, which defines the address of the
websocket server. You should change neither `websocket-server` nor `href-base`,
just the underlying `base`.

### password

Points to the file with user accounts and passwords (see
[pass.json](#pass.json)).

### anno-config 

Top-level Dhall configuration file. See also the [dhall](#dhall) section.


## Dhall

A more fine-grained configuration is stored in the form of a [Dhall][dhall]
file. It contains, notably:

* A list of non-terminal and terminal categories which can be assigned to
  syntactic nodes.
* Definitions of the annotation *entities*, i.e., the objects with which the nodes
  in syntactic trees can be marked. The corresponding attributes, the
  attributes' types, and potential values, are also defined via Dhall.
* Definitions of the annotation *relations* and the corresponding attributes,
  i.e., the objects which can connect two different nodes belonging to two
  different syntactic trees.
* Definitions of annotation command invocations: the corresponding menu items,
  command-line instructions, keyboard shortcutes, help messages, and so on.
* Definitions of *annotation levels* (e.g., syntax, termporal, etc.).

## User accounts and passwords

The `pass.json` file keeps information about the annotation users and their
passwords.  The initial `pass.json` file defines two users:

* Admin: login = `admin`, password = `admin`,
* Guest: login = `guest`, password = `guest`.

Of course you should change the administrator's password immediately after
setting up your own instance. Both the `admin` and the `guest` accounts are
special and contain the corresponding entries in the top-level
[devel.cfg](#devel.cfg) configuration file.

New accounts can be added via the web interface, once you log in as the
website's administrator.



[dhall]: https://github.com/dhall-lang/dhall-lang "Dhall"
