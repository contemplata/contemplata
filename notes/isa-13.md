Annotation tool
---------------

Anything that we would like to write about the annotation tool:
* We could start by a description of how the tool works / looks like 
  from the perspective of the annotator.  We could also give 
  a screenshot at this point.  It should be emphasized that the tool
  allows syntactic annotation and temporal annotation in parallel.
* Description of the architecture could follow here: client/server architecture
  (+ a database), the technology used (Elm, haskell).
* The fact that it's online should eventually allow a more collaborative
  annotation style; besides, (which is less important, I suppose,)
  this facilitates the installation process (the tool can be used on
  any machine supplied with a standard internet browser)
* It would be nice to mention this: we would like to make the "collaboration"
  between the annotator and the syntactic parser more interactive.
  The idea is that, e.g., the annotator be able to select parts of the
  syntactic structure as correct and ask the parser to provide the most plausible
  syntactic structure which satisfies the user-specified constraints.


Text
----

All of the manual annotation phases are conducted using a single annotation
tool, which has been specificially implemented for the purpose of the project.

Fig.~X shows a screenshot of the tool's workspace, which consists of two
vertically arranged annotation windows. Duplication of the annotation space
allows, in particular, to easily view and edit the termporal relations occuring
between different trees. A link between two nodes is created by selecting the
two nodes and using the corresponding command.

As mentioned in the previous section, one of the primary functionalities
provided by the tool is to allow correction of syntactic trees. To this end, the
tool allows to perform several structure-modifing operations: adding and
deleting nodes, chaning the parent of a node, chaning the position of the node
w.r.t. its parent, etc. Only the operations which preserve the well-formedeness
of syntactic structures are allowed. Non-projective trees, which may result from
the position-changing operation, are on the other hand allowed.

The tool is implemented in a client/server architecture: the frontend annotation
tool, written in the Elm language, runs in any modern internet browser and
communicates with the Haskell-based server via websockets.
