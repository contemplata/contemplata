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
          <img src="/public/img/guide/syntax/workspace.png" alt="Workspace" style="width:75%">
          <figcaption>Annotation mode</figcaption>
        </figure>
      </center></p>

      <h4>Navigation in the annotation mode</h4>
      <p>
        To navigate between the turns in the annotation mode, you can either
        click on a particular turn in the <b>Context</b> side window, or press
        <em>Page Up</em> and <em>Page Down</em> to move up and down in the
        file, respectively.
      </p>
      <p>
        Once you find the sentence you wish to annotate, you can slide the
        syntactic tree in different directions by left-clicking anywhere in the
        main workspace and sliding the mouse, while keeping the left mouse
        button pressed.
      </p>

      <h4>Node selection</h4>
      <p>
        There are two types of selection: the main selection and the additional
        selection. Some operations only take the main selection into account,
        while others will be applied over all selected nodes (e.g. the <a
        href="/user/guide#addnode">Add</a> and <a
        href="/user/guide#delnode">Delete</a> commands).
      </p>
      <p>
        To (main) select a node, it is sufficient to click on it. Additional
        selection is obtained by clicking on a node with CTRL pressed. Several
        additional nodes can be selected at the same time.
      </p>

      <h4>Commands</h4>
      <p>
        Three types of commands are available. The menu commands are visible in
        the top-left corner of the main annotation workspace. The first two
        commands are generic -- <b>Menu</b> and <b>Save</b>, which allow to go
        to the main menu and to save the currently annotated file, respectively.

        The other menu commands are divided into three annotation levels:
        <b>Segmentation</b>, <b>Syntax</b>, and <b>Temporal</b>. You can click
        (or CTRL+click) on the name of the current level to change it. The
        commands from the other levels still can be used via the command line or
        keyboard shortcuts, though.
      </p>
      <p>
        Some commands are available via keyboard shortcuts, which are typically
        indicated in the names of their corresponding menu commands as
        underlined characters. For instance, the <b>Parse</b> command from the
        syntactic annotation level can be run with the <b>p</b> keyboard
        shortcut.
      </p>
      <p>
        The third type of commands can be run via command line. After pressing
        <b>Space</b>, you will see the list of available commands in this mode.
        To run a particular command, just type it and press <b>Enter</b>. You
        can also use <b>Tab</b> for autocompletion. Besides, you don't have to
        type the entire command, just a prefix which uniquely identifies it (for
        instance, <b>sa</b> for <b>save</b>).
      </p>
      <div class="row">
        <div class="col-sm-2"/>
        <div class="col-sm-8">
          <figure><center>
            <img src="/public/img/guide/general/command-line.png" alt="Command line" style="width:100%">
            <figcaption>Command line, placed on the bottom of the main
            workspace</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-2"/>
      </div>

      <h4>Undo/redo</h4>
      <p>
        All the operations which modify the underlying file can be undone. To do
        that, press <b>CTRL+z</b>. Conversely, to perform redo, press <b>z</b>.
      </p>

      <h4>Resizing</h4>
      <p>
        The relative size of the workspaces and the side windows can be changed
        via the keyboard arrows: up, down, left, and right.
      </p>
      <p>
        To change the size of the syntactic tree, sentences shown in the
        <b>Context</b> window, etc., use <b>CTRL + mouse scroll</b> to zoom
        either in or out (note that this is a built-in browser behavior; other
        built-in ways of zooming -- CTRL + plus and CTRL + minus -- are not
        available for the moment, these key combinations are intercepted by
        Contemplata, but the tool does not propagate them up).
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
        <img src="/public/img/guide/syntax/joining-turns.png" alt="Join turns" style="width:75%">
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
            <img src="/public/img/guide/syntax/joining-turns-result.png" alt="Result of joining turns" style="width:75%">
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
        <img src="/public/img/guide/syntax/remove-subtree.png" alt="Remove subtree" style="width:75%">
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
          <img src="/public/img/guide/syntax/split-sentence.png" alt="Split sentence" style="width:75%">
          <figcaption>Marking the terminal nodes before sentence split</figcaption>
        </figure>
      </center></p>

      <p><center>
        <figure>
          <img src="/public/img/guide/syntax/split-sentence-result.png" alt="Split sentence" style="width:75%">
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
        <img src="/public/img/guide/syntax/join-words.png" alt="Join words" style="width:75%">
        <figcaption>Joining words</figcaption>
      </center></figure>

      <h4>Split words</h4>
      <p>
        The inverse operation of word splitting is available via the <b>Split
        word</b> menu command (<b>splitword</b> from command line), which can be
        run after the terminal node to be split has been selected.
      </p>
      <figure><center>
        <img src="/public/img/guide/syntax/split-word.png" alt="Split word" style="width:75%">
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
        <img src="/public/img/guide/syntax/dummify.png" alt="Dummify" style="width:75%">
        <figcaption>A situation where the entire tree should be removed with
        <b>Dummify</b></figcaption>
      </center></figure>
    </div>
  </div>


  <div class="panel panel-default" id="syntax">
    <div class="panel-heading">Syntax</div>
    <div class="panel-body">

      <h4 id="addnode">Add node</h4>
      <p>
        The <b>Add</b> command (<b>addnode</b> from command line) serves to add
        a new node over the selected node(s). The label assigned to the new node
        is <em>?</em> by default and should be changed to respect the tagset.
      </p>
      <div class="row">
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/addnode.png" alt="Add node input" style="width:100%">
            <figcaption>Input for the <b>Add</b> command</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/addnode-result.png" alt="Add node result" style="width:100%">
            <figcaption>The result of adding a new node</figcaption>
          </center></figure>
        </div>
      </div>

      <h4>Edit</h4>
      <p>
        The label assigned to the selected node can be changed via the
        <b>Edit</b> side window, which can be quickly reached via the <b>e</b>
        keyboard shortcut.
      </p>
      <div class="row">
        <div class="col-sm-3"/>
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/edit.png" alt="Edit" style="width:100%">
            <figcaption>Editing node labels</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-3"/>
      </div>
      <p>
        Note that there is also a <em>Comment</em> field below the
        <em>Label</em> field in the <b>Edit</b> side window. You can use it,
        e.g., to provide information related to the certainty of your annotation
        of the node or its subtree.
      </p>

      <h4 id="delnode">Remove node</h4>
      <p>
        To remove a particular node, select it and use the <b>Delete</b> command
        (<b>delnode</b> from command line).
      </p>

      <h4>Reattach</h4>
      <p>
        To change the parent of a particular node (and its subtree), (i) select
        the node which should be displaced, (ii) CTRL+select the new parent
        node, and (iii) press <b>r</b>.
      </p>
      <div class="row">
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/reattach.png" alt="Reattach" style="width:100%">
            <figcaption>Selecting nodes for reattachment</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/reattach-result.png" alt="Reattach result" style="width:100%">
            <figcaption>The result of reattachment</figcaption>
          </center></figure>
        </div>
      </div> 

      <h4>Parse</h4>
      <p>
        To reparse the currently sentence (e.g. after some changes in
        segmentation), use the <b>Parse</b> menu command (<b>parse</b> from
        command line). Note that, in case the current tree contains several
        SENT-rooted subtrees (i.e., several sub-sentences), only the selected
        subtrees (i.e., those with at least one selected node) will be
        re-parsed. If no node is selected, all sub-sentences will be re-parsed.
      </p>

      <h4>Parse without changing POS tags</h4>
      <p>
        Similar to <b>Parse</b>, the <b>CTRL+Parse</b> menu command
        (<b>parsepos</b> from command line) allows to reparse the current
        sentence, but it does not allow the underlying parser to change the POS
        tags.
      </p>
      <div class="row">
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/parsepos.png" alt="Parse without chaing POS tags" style="width:100%">
            <figcaption>Input for parsing: <em>numéro</em> and
            <em>théléphone</em> both marked as nouns without specific POS
            subcategories</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/syntax/parsepos-result.png" alt="Parse without chaing POS tags: the result" style="width:100%">
            <figcaption>The result: <em>numéro de théléphone</em> analyzed as a
            MWE</figcaption>
          </center></figure>
        </div>
      </div> 
      <p>
        <b>NOTE:</b> for this command to work correctly, each pre-terminal node
        has to have a proper POS tag and a <b>single terminal child</b>.
      </p>
      
      <h4>Non-projective trees</h4>
      <p>
      </p>

    </div>
  </div>

  <div class="panel panel-default" id="entities">
    <div class="panel-heading">Temporal entities</div>
    <div class="panel-body">

      <h4>Marking events, signals, and temporal expressions</h4>
      <p>
        To annotate the selected node as an event, use the <b>Event</b> menu
        command.

        Similarly, the <b>Signal</b> menu command allows to annotate signals, and
        the <b>Timex</B> menu command -- temporal expressions.
      </p>
      <p>
        Note that, after you mark a particular node as a signal/event/timex, a
        set of dedicated attributes becomes available in the <b>Edit</b> side
        window, as shown in the example below.
      </p>
      <div class="row">
        <div class="col-sm-3"/>
        <div class="col-sm-6">
          <figure><center>
            <img src="/public/img/guide/temporal/attributes.png" alt="Attributes" style="width:100%">
            <figcaption>Attributes available for annotation for the selected event</figcaption>
          </center></figure>
        </div>
        <div class="col-sm-3"/>
      </div>

      <h4>Anchoring</h4>
      <p>
        Most of the attributes have a prescribed, closed set of possible values.
        There are some which perform the function of relations between the
        selected node (of which the attributes are shown in the <b>Edit</b>
        window) and anothe node. For instance, each Timex can be anchored at
        another temporal entity, as shown below.
      </p>
      <p>
        TODO: need a good example!
      </p>

    </div>
  </div>

  <div class="panel panel-default" id="relations">
    <div class="panel-heading">Temporal relations</div>
    <div class="panel-body">
      <p>
        For the moment, there is only one way to create untyped relations:
        choose one node in each of the workspaces, top and bottom, and run the
        <b>connect</b> command line command. An untyped link between the two
        nodes (which can belong to two different trees) will be created. The
        direction of the link is alwas from the top tree to the bottom one. It
        can be deleted by selecting the circular node in the middle of the
        relation and pressing <b>d</b>.
      </p>

      <p>More to come...</p>
    </div>
  </div>

</apply>
