<div class="panel panel-default" id="syntax">
  <!--div class="panel-heading">Syntax</div-->
  <div class="panel-body">

    <p>
      The goal of this annotation stage is to assign the correct syntactic
      structure (constituency tree) to the given sentence and to assign the
      correct labels (part-of-speech tags and phrasal labels) to the tree's
      individual nodes. Note that the trees assigned by default to the
      individual speech turns were obtained with the Stanford parser.
    </p>

    <p>
      At this point, we assume that the sentence is already correctly tokenized
      and that the pragmatic expressions irrelevant for semantic annotation are
      filtered out. If this is not the case, you should be probably looking at
      the part describing the <a href=user/guide#preprocessing>preprocessing</a>
      annotation stage.
    </p>

    <h4 id="parse">Parse</h4>
    <p>
      The <b>Parse</b> operation takes the list of tokens in the current tree
      and re-analyzes them syntactically using the Stanford parser. It can be
      useful after <a href=user/guide#preprocessing#word-split>word spitting</a>
      and other changes in segmentation.
    </p>
    <p>
      To perform the operation, use the <b>Parse</b> menu command (<b>parse</b>
      from command line). Note that, in case the current tree contains several
      SENT-rooted subtrees (i.e., several sub-sentences), only the selected
      subtrees (i.e., those with at least one selected node) will be re-parsed.
      If no node is selected, all sub-sentences will be re-parsed.
    </p>
    <p>
      <b>WARNING</b>: This operation destroys all the modifications of syntactic
      structure you have applied so far.
    </p>

    <h4 id="edit">Edit</h4>
    <p>
      The label assigned to the main selected node can be changed via the
      <b>Edit</b> side window, which can be quickly reached via the <b>e</b>
      keyboard shortcut.

      Edition can be used, among others, to change the POS tags assigned to the
      individual words before running the
      <a href=user/guide#syntax#parsepos>POS-preserving parsing command</a>.
    </p>
    <div class="row">
      <div class="col-sm-3"/>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/edit.png" alt="Edit" style="width:100%">
          <figcaption>Editing node labels</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-3"/>
    </div>
    <p>
      Note that there is also a <em>Comment</em> field below the <em>Label</em>
      field in the <b>Edit</b> side window. You can use it, e.g., to provide
      information related to the certainty of your annotation of the node or its
      subtree.
    </p>

    <h4 id="parsepos">Parse without changing POS tags</h4>
    <p>
      This operation takes the list of tokens in the current tree, <b>together
      with the corresponding POS tags</b>, and uses the underlying parser to
      re-analyze them. In contrast to <a href=user/guide#syntax#parse>Parse</a>,
      the parser is <em>not</em> allowed to change the POS tags.
    </p>
    <p>
      The operation can be launched with the <b>CTRL+Parse</b> menu command
      (<b>parsepos</b> from command line).
    </p>
    <p>
      It is adviced to invoke this operation once you have finished to valide
      all the POS tags of the considered speech turn. Then one can expect the
      resulting parse tree to be more accurate, as presented in the example
      below.
    </p>
    <div class="row">
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/parsepos.png" alt="Parse without changing POS tags" style="width:100%">
          <figcaption>Input for parsing: <em>numéro</em> and
          <em>théléphone</em> both marked as nouns without specific POS
          subcategories</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/parsepos-result.png" alt="Parse without chaing POS tags: the result" style="width:100%">
          <figcaption>The result: <em>numéro de théléphone</em> analyzed as a
          MWE</figcaption>
        </center></figure>
      </div>
    </div> 
    <p>
      <b>WARNING</b>: For this command to work correctly, each pre-terminal node
      has to have a <b>proper POS tag</b> and a <b>single terminal child</b>.

      The latter property is not guaranteed to be preserved by all Contemplata
      operations. For instance, the <a
      href=user/guide#preprocessing#word-split>word splitting</a> operation will
      result in a POS pre-terminal node with several terminal children.
    </p>
    <p>
      <b>WARNING</b>: Just as <a href=user/guide#syntax#parse>Parse</a>, this
      operation destroys the manual modifications of the syntactic structure.
    </p>

    <h4 id="addnode">Add node</h4>
    <p>
      Comtemplata provides several basic tree modification operations: adding a
      new node, deleting existing nodes or subtrees, re-attaching subtrees, etc.
      While individually quite simple, together they should provide a
      sufficiently rich toolset for efficiently correcting syntactic trees.
    </p>
    <p>
      The <b>Add</b> command (<b>addnode</b> from command line) serves to add a
      new node over the selected node(s). The label assigned to the new node is
      set to <em>?</em> by default and should be changed (via <a
      href=user/guide#syntax#edit>Edit</a>) to respect the tagset.
    </p>
    <div class="row">
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/addnode.png" alt="Add node input" style="width:100%">
          <figcaption>Input for the <b>Add</b> command</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/addnode-result.png" alt="Add node result" style="width:100%">
          <figcaption>The result of adding a new node</figcaption>
        </center></figure>
      </div>
    </div>

    <h4 id="delnode">Remove node</h4>
    <p>
      To remove a particular node, select it and use the <b>Delete</b> command
      (<b>delnode</b> from command line).
    </p>

    <h4 id="deltree">Remove subtree</h4>
    <p>
      To remove the subtree of the selected node (including the node itself),
      use the <b>CTRL+d</b> keyboard shortcut, the <b>CTRL+Delete</b> menu
      command, or <b>deltree</b> from command line. An important consequence of
      the operation is that all the tokens corresponding to the terminal nodes
      in the subtree will be removed.

      Note that a subtree can be only removed if the resulting syntactic tree is
      well structured. Otherwise, the tree remains unchanged.
      <!--The figure below on the left shows a situation where performing the
      command is not allowed, because it . Removing the VN node itself with its
      subtree is, on the other hand, perfectly fine.-->
    </p>
    <div class="row">
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/remove-subtree-forbidden.png" alt="Remove subtree forbidden" style="width:100%">
          <figcaption>Removing the subtree of the selected node not allowed,
          because it would lead to a tree with a non-terminal (I)
          leaf.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/remove-subtree-allowed.png" alt="Remove subtree allowed" style="width:100%">
          <figcaption>Removing the selected subtree allowed. The result will
          contain neither the non-terminal <em>I</em> nor its subtree consisting
          of a single terminal <em>oh</em>.</figcaption>
        </center></figure>
      </div>
    </div> 

    <h4>Reattach</h4>
    <p>
      It is possible that the choice of the parent of a particular node (and its
      subtree) is incorrect, as a result of an erroneous syntactic analysis.

      To change the parent of a particular node (and its subtree), (i) select
      the node which should be displaced, (ii) CTRL+select the new parent node,
      and (iii) press <b>r</b>.
    </p>
    <div class="row">
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/reattach.png" alt="Reattach" style="width:100%">
          <figcaption>Selecting nodes for reattachment</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/reattach-result.png" alt="Reattach result" style="width:100%">
          <figcaption>The result of reattachment</figcaption>
        </center></figure>
      </div>
    </div> 


    <h4>Non-projective trees</h4>
    <p>
      The selected node, together with its subtree, can me moved left or right
      w.r.t. its sister nodes by pressing <b>CTRL+left</b> or <b>CTRL+right</b>
      keyboard shortcuts, respectively. This will lead to a non-projective tree.
      Contemplata tries to identify the non-projective fragments and draw them
      in rose, as shown in the example below.
    </p>
    <p>
      <b>TODO:</b> we need a better example here, since this sentence should not
      be analyzed with a non-projective tree.
    </p>
    <div class="row">
      <div class="col-sm-2"/>
      <div class="col-sm-8">
        <figure><center>
          <img src="public/img/guide/syntax/non-projective.png" alt="Non projective tree" style="width:100%">
          <figcaption>A non-projective tree, with the non-projective fragment marked in rose</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-2"/>
    </div>
    <p>
      <b>WARNING</b>: The current limitation is that, once you create a
      non-projective tree, the permuted sentence can be no longer re-parsed with
      the underlying parser. Some other operations, after which parsing is
      performed by default, will no longer work correctly either. You should
      therefore create non-projective structures only at the end of the
      syntactic annotation of a given sentence.
    </p>

  </div>
</div>
