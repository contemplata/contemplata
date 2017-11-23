<apply template="default">

  <div class="well"><em>
    <p>This is a guide to annotation of the ODIL files with Contemplata.</p>

    <p>
      Annotation is performed at different levels:
      <a href="/user/guide#segmentation">segmentation</a>,
      <a href="/user/guide#syntax">syntax</a>,
      <a href="/user/guide#entities">termporal entities</a>, and
      <a href="/user/guide#relations">termporal relations</a>.
      A dedicated section to each of these levels can be found below.
    </p>
  </em></div>

  <div class="panel panel-default" id="general">
    <div class="panel-heading">General information</div>
    <div class="panel-body">

      <h4>Tableau de bord</h4>
      <p>
        You can find the list of ODIL files at the <a
        href="/user/files">files</a> subpage (see menu). The files listed in the
        <b>Write</b> section are for you to annotate. They are divided in three
        categories. The <b>Waiting</b> and <b>In progress</b> files are the
        files waiting for annotation and the files you started annotating,
        respectively. When you finish annotating a file, click on <b>finish</b>
        to move it to the <b>Done</b> section. Files gathered there can no loger
        be modified. You can also click on <b>postpone</b> to revert the file to
        the waiting list.
      </p>

      <p>
        As for the files to which you only have the <b>Read</b> access, they are
        either for others to annotate, or left in the database for archival
        purposes. Still, they can serve you as a reference for comparison with
        your own annotations.
      </p>

      <h4>Annotation mode</h4>
      <p>
        When you click on a particular file, you will be moved to the annotation
        mode. It consists of two annotation workspaces, top and bottom, each
        showing a syntactic tree assigned to (a) particular speech turn(s) in
        the file. The side window, placed on the right, can be switched between:
        (i) the <b>Context</b>, showing the list of the speech turns in the
        file, with the turn being annotated marked in bold, (ii) the <b>Edit</b>
        mode, showing the various attributes which can be annotated for the
        selected tree node, and (iii) the <b>Messages</b> received from the
        server.
      </p>

      <p><center>
        <figure>
          <img src="/public/img/guide/workspace.png" alt="Workspace" style="width:75%">
          <figcaption>Annotation mode</figcaption>
        </figure>
      </center></p>

      <h4>Navigation in the annotation mode</h4>
      <p>
         To navigate between the turns in the annotation mode you can either
         click on a particular turn in the <b>Context</b> side window, or press
         <em>Page Up</em> and <em>Page Down</em> to move up and down in the
         file, respectively.
      </p>

      <h4>Node selection</h4>
      <p>
         Stub.
      </p>

      <h4>Commands</h4>
      <p>
         Stub.
      </p>

      <h4>Undo/redo</h4>
      <p>
         Stub.
      </p>

      <h4>Resizing windows</h4>
      <p>
         Stub.
      </p>

    </div>
  </div>

  <div class="panel panel-default" id="segmentation">
    <div class="panel-heading">Segmentation</div>
    <div class="panel-body">

      <p>
        In many cases there is no need to modify the segmentation, already
        performed correctly by the underlying syntactic parser. For the other
        situation, several segmentation-related operations are available.
      </p>

      <h4>Merging turns</h4>

      <p>
        Sometimes a syntactically coherent portion of speech is devided between
        several speech turns in the file, as in the following example:
      </p>

      <figure><center>
        <img src="/public/img/guide/joining-turns.png" alt="Join turns" style="width:75%">
        <figcaption>An utterance divided between several speech turns</figcaption>
      </center></figure>

      <p>
        You can merge the currently selected turn with another turn by
        CTRL+clicking on the latter in the context side window. This process can
        be iterated to join more than two turns together, as shown below.
      </p>

      <p>
        <center>
          <figure>
            <img src="/public/img/guide/joining-turns-result.png" alt="Result of joining turns" style="width:75%">
            <figcaption>A tree corresponding to several speech turns after their
            merge</figcaption>
          </figure>
        </center>
      </p>

      <h4>Restart with preprocessing</h4>
      <p>
        To syntactically annotate the given sentence from scratch, use the
        <b>CTRL+Restart</b> menu command (<b>:restartpreproc</b> from command
        line). It restores all the tokens, performs pre-processing, and uses the
        parser to analyse the sentence.

        It may be useful, in particular, after the merging operation, which
        results in several SENT-rooted syntactic trees. Restart, then,
        re-analyses the entire sentence as one syntactic tree.
      </p>

      <h4>Restoring tokens</h4>
      <p>
        To restore a token wrongly removed during the pre-processing step (such
        tokens are marked in grey), just CTRL+click on it in the context side
        window of the current workspace. You can also use the <b>Restart</b>
        menu command (<b>:restart</b> from command line) to restore all the
        tokens and re-parse the resulting sentence.
      </p>

      <h4>Removing tokens</h4>
      <p>
        Tokens can be manually removed in the main annotation workspace. Select
        the root of the tree to be removed and use the <b>CTRL+d</b> keyboard
        shortcut, the <b>CTRL+Delete</b> menu command (currently in the
        <em>Syntax</em> annotation mode), or <b>delnode</b> from command line.
        As a result, all the tokens corresponding to the terminal nodes in the
        tree will be removed.

        Note that a subtree can be only removed if the resulting syntactic tree
        is well structured. The following example shows a situation where
        performing the command is not allowed, becuase it would lead to a tree
        with a non-terminal (VN) leaf. Removing the VN node itself with its
        subtree is, on the other hand, perfectly fine.
      </p>
      <figure><center>
        <img src="/public/img/guide/remove-subtree.png" alt="Remove subtree" style="width:75%">
        <figcaption>A context in which removing the subtree of the selected node
        is not allowed</figcaption>
      </center></figure>

      <h4>Split sentence</h4>
      <p>
        Just as it is possible to merge several speech turns, it is possible to
        split a given sentence in several sub-sentences. In this case, however,
        the currently annotated speech turn is not modified, it's just the
        syntactic tree which is divided into several SENT-rooted sub-trees.

        In order to perform the operation, first select the terminal nodes which
        mark the first words of the individual sub-sentences, and then run the
        <b>Split sentence</b> menu command (<b>:splitsent</b> from command
        line), as shown below.
      </p>

      <p><center>
        <figure>
          <img src="/public/img/guide/split-sentence.png" alt="Split sentence" style="width:75%">
          <figcaption>Marking the terminal nodes before sentence split</figcaption>
        </figure>
      </center></p>

      <p><center>
        <figure>
          <img src="/public/img/guide/split-sentence-result.png" alt="Split sentence" style="width:75%">
          <figcaption>The result of sentence split</figcaption>
        </figure>
      </center></p>
        
      <h4>Join words</h4>
      <p>
        When the parser incorrectly splits a token into several ones, as in the
        example below, you can select the terminal nodes corresponding to the
        tokens that should be joined, and run the <b>Join words</b> menu command
        (<b>joinwords</b> from command line).
      </p>
      <figure><center>
        <img src="/public/img/guide/join-words.png" alt="Join words" style="width:75%">
        <figcaption>Joining words</figcaption>
      </center></figure>

      <h4>Split words</h4>
      <p>
        The inverse operation of word splitting is available via the <b>Split
        word</b> menu command (<b>splitword</b> from command line), which can be
        run after the terminal node to be split has been selected.
      </p>
      <figure><center>
        <img src="/public/img/guide/split-word.png" alt="Split word" style="width:75%">
        <figcaption>A token which should be split into three separate tokens:
        <em>lui</em>, <em>-</em>, and <em>même</em>
        </figcaption>
      </center></figure>

      <h4>Dummify</h4>
      <p>
        Note that the delete tree command does not allow to remove the entire
        tree, as the underlying model considers such a tree as invalid. In
        certain situation, however, none of the tokens is relevant, as in the
        example below. Thus we adopt a convention in which a tree with a single
        ROOT and an empty terminal node represents an empty tree. You can use
        the <b>Dummify</b> menu command (<b>dummify</b> from command line) to
        obtain it.
      </p>
      <figure><center>
        <img src="/public/img/guide/dummify.png" alt="Dummify" style="width:75%">
        <figcaption>A situation where the entire tree should be removed with
        <b>Dummify</b></figcaption>
      </center></figure>

    </div>

    
  </div>

  <div class="panel panel-default" id="syntax">
    <div class="panel-heading">Syntax</div>
    <div class="panel-body">
      This is a stub.
    </div>
  </div>

  <div class="panel panel-default" id="entities">
    <div class="panel-heading">Termporal entities</div>
    <div class="panel-body">
      This is a stub.
    </div>
  </div>

  <div class="panel panel-default" id="relations">
    <div class="panel-heading">Termporal relations</div>
    <div class="panel-body">
      In preparation.
    </div>
  </div>

</apply>
