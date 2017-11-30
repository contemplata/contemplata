<div class="panel panel-default" id="syntax">
  <div class="panel-heading">Syntax</div>
  <div class="panel-body">

    <p>
      The goal of this annotation stage is to assign the correct syntactic
      structure (constituency tree) to the given sentence and to assign the
      correct labels (part-of-speech tags and phrasal labels) to the tree's
      individual nodes. The trees assigned by default to the individual speech
      turns were obtained with the Stanford parser.
    </p>

    <p>
      At this point, we assume that the sentence is already correctly tokenized
      and that the pragmatic expressions irrelevant for semantic annotation are
      filtered out. If this is not the case, you should be probably looking at
      the part describing the <a href=user/guide#segmentation>pre-processing</a>
      annotation stage.
    </p>

    <h4>Edit</h4>
    <p>
      The label assigned to the selected node can be changed via the <b>Edit</b>
      side window, which can be quickly reached via the <b>e</b> keyboard
      shortcut.
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
      Note that there is also a <em>Comment</em> field below the
      <em>Label</em> field in the <b>Edit</b> side window. You can use it,
      e.g., to provide information related to the certainty of your annotation
      of the node or its subtree.
    </p>

    <h4 id="addnode">Add node</h4>
    <p>
      The <b>Add</b> command (<b>addnode</b> from command line) serves to add
      a new node over the selected node(s). The label assigned to the new node
      is <em>?</em> by default and should be changed to respect the tagset.
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

    <h4>Reattach</h4>
    <p>
      To change the parent of a particular node (and its subtree), (i) select
      the node which should be displaced, (ii) CTRL+select the new parent
      node, and (iii) press <b>r</b>.
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

    <h4 id="parse">Parse</h4>
    <p>
      The <b>Parse</b> operation takes the list of tokens in the current tree
      and re-analyzes them syntactically using the Stanford parser. It can be
      useful after spitting (TODO: href) or other changes in segmentation.
      TODO: move to pre-processing?
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

    <h4 id="parsepos">Parse without changing POS tags</h4>
    <p>
      Similar to <b>Parse</b>, the <b>CTRL+Parse</b> menu command
      (<b>parsepos</b> from command line) allows to reparse the current
      sentence, but it does not allow the underlying parser to change the POS
      tags.
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
      <b>NOTE:</b> for this command to work correctly, each pre-terminal node
      has to have a proper POS tag and a <b>single terminal child</b>.
    </p>
    
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
