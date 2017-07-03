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

% All of the manual annotation phases are conducted using a single annotation
% tool, which has been specificially implemented for the purpose of the project.
% 
% Fig.~X presents a screenshot of the tool's workspace, which consists of two
% vertically arranged annotation windows, showing two syntactic trees assigned to
% two (typically different) sentences in a given file. As mentioned in the
% previous section, one of the primary functionalities provided by the tool is to
% allow correction of syntactic trees. To this end, the tool allows to perform
% several structure-modifing operations: adding and deleting nodes, changing the
% parent of a node, changing the position of the node w.r.t. its parent, etc. Only
% the operations which preserve the well-formedeness of syntactic structures are
% allowed.
% 
% % Non-projective trees, which may result from the position-changing operation,
% % are on the other hand allowed.
% 
% Duplication of the annotation workspace facilitates, among others, viewing and
% editing the termporal relations occuring between different trees. Such relations
% can be created by selecting the corresponding nodes and using an appropriate
% keyboard command. The newly created events and temporal relations are supplied
% with default IOS-TimeML-related attribute values, which can be subsequently
% changed manually in the side windows (on the right).
% 
% The tool is implemented in a client/server architecture. The frontend annotation
% tool is written in the Elm language, which compiles to JavaScript and can be
% thus used in any modern internet browser. The client communicates with a Haskell
% server via websockets. The server, in turn, has access to the database in which
% the annotated files are stored.
% 
% Such an architecture has a couple of advantages. For instance, the annotator
% does not have to install anything locally, and the server can provide the user
% with more advanced functionalities, e.g., re-parse a given sentence with
% additional constraints supplied by the annotator. In the long run, the
% client/server architecture should also allow a more collaborative annotation
% style.

All of the manual annotation phases are conducted using a single annotation
tool, which has been specificially implemented for the purpose of the project.

The tool's workspace consists of two vertically arranged annotation windows,
showing two syntactic trees assigned to two (typically different) sentences in a
given file. As mentioned in the previous section, one of the primary
functionalities provided by the tool is to allow correction of syntactic trees.
To this end, the tool allows to perform several structure-modifing operations:
adding and deleting nodes, changing the parent of a node, changing the position
of the node w.r.t. its parent, etc. Only the operations which preserve the
well-formedeness of syntactic structures are allowed.

Duplication of the annotation workspace facilitates, among others, viewing and
editing the termporal relations occuring between different trees. Such relations
can be created by selecting the corresponding nodes and using an appropriate
keyboard command. The newly created events and temporal relations are supplied
with default ISO-TimeML-related attribute values, which can be subsequently
changed manually in the side windows. We plan to later experiment with a
semi-automatic annotation of the attribute values, where the corresponding
machine-learning annotation model is being gradually bootstrapped from the
already annotated part of the corpus.

The tool is implemented in a client/server architecture. The frontend annotation
tool is written in Elm (\url{http://elm-lang.org/}), which compiles to
JavaScript, thus the tool can be used in any modern internet browser. The client
annotation tool communicates with a Haskell server via websockets, with the
annotated files serialized to JSON before being sent. On both sides, annotation
data is represented with appropriate data types, which guarantees, among others,
that malformed data is never sent to the server to be stored in the database.

Such an architecture has a couple of advantages. For instance, the annotator
does not have to install anything locally, and the server can provide the user
with more advanced functionality. For instance, the server can be requested to
syntactically re-analyze a given sentence in a way which takes the constraints
specified directly by the annotator (e.g. a particular tokenization) into
account. In the long run, the client/server architecture should also allow a
more collaborative annotation style.
