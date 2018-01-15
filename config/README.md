TODO: change the description below.

The above command copies the initial password file (`pass.json`), where the
passwords of the website administrator and the guest user are set (initially to
`admin` and `guest`, respectively -- TODO: check). It also copies the high-level
configuration file `devel.cfg`, in which the following variables are set:

* `DB` -- path to the database directory 
* `href-base` -- the base URL of the website
* `password` -- password file
* `websocket-server` -- address of the websocket server (TODO: should be simply
  set to `$href-base/ws`?)
* `anno-config` -- top-level Dhall configuration file (TODO: send to a section
  about the Dhall configuration)

The values of the remaining variables can be normally left as they are.
