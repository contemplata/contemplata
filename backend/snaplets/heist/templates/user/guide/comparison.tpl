<div class="panel panel-default" id="syntax">
  <div class="panel-body">

    <h4>Comparison and adjudication</h4>
    <p>
      Contemplata allows to compare and annotate several files at the same time.
      See <a href=user/guide#general#tableau>tableau de bord</a> for information
      on how to enter the comparison mode. Once you do that, you will remark
      that the names of all the selected files are shown at the bottom of each
      workspace. You can click on the name of the file to switch to it.
    </p>
    <!--p>
      The annotations performed in the previously annotated file are not
      discarded in this case, but they are not stored in the database either.
      You might need to switch back to the previous file and <b>Save</b> the
      changes before you quit annotation.
    </p-->
    <p>
      Contemplata provides a comparison operation (<b>compare</b> from command
      line) which searches for differences between the trees in the top and the
      bottom workspaces. As a result of the operation, all the nodes that are
      present in one tree but not the other (or have a different set of
      attributes) will be highlighted in red (i.e. selected), as exemplified
      below. If the two trees are identical, the tool automatically searches for
      differences in the subsequent trees.
    </p>
    <div class="row">
      <div class="col-sm-3"/>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/comparison.png" alt="Comparison"
          style="width:100%"> <figcaption>The result of the comparison of two
          syntactic trees. The nodes which do not have a direct counterpart in
          the alternative tree get marked in red.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-3"/>
    </div>
    <p>
      The comparison operation searches for the differences not only at the
      segmentation and syntactic levels, but also at the levels of temporal
      entities and relations. When two trees are identical at all levels but the
      level of relations, the nodes with different sets of ingoing and/or
      outgoing relations will be selected.
      <!--The attributes assigned to the relations, as well as attributes'
      values, are also taken into account when searching for differences.-->
    </p>

  </div>
</div>