<div class="panel panel-default" id="relations">
  <!--div class="panel-heading">Temporal relations</div-->
  <div class="panel-body">
    <h4 id="types">Types</h4>
    <p>
      Contemplata allows to draw typed relations between tree nodes. Following
      the ISO TimeML recommandation, In the Temporal@ODIL project, four types of
      relations can be created: TLink, SLink, ALink and MLink (please refer to
      the annotation guidelines or the ISO TimeML norm for details on these
      relations types). The commands responsible for creating the relations of
      each of these types can be found in the menu (the <em>Temporal</em>
      section).
    </p>
    <p>
      Contemplata uses the convention that relations are always created and
      displayed from the top to the bottom workspace, even those which are
      internal to a single tree.

      While relations could be shown in both directions, showing relations
      between two windows allows to avoid unnecessary clutter which would arise
      when many relations hold between the trees in focus.
    </p>
    <h4 id="create">Create</h4>
    <p>
      To create a new relation, select a main node in each of the two workspaces
      and use one of the related commands (<em>SLink</em>, <em>TLink</em>,
      etc.). Be aware that the temporal relations are directed one. You must
      therefore be careful to the order of the related nodes. The new relation
      you create will lead from (origin) the node selected in the top workspace
      to (target) the one the bottom workspace.
    </p>
    <p>
      The resulting relation is represented with a dashed line, with a grey node
      in the middle of the line indicating its type. This node allows to select
      the relation for the subsequent operations (e.g., <a
      href=user/guide#relations#attributes>defining/modification of attribute
      values</a>).
    </p>
    <p>
      <b>WARNING:</b> the above procedure (i.e., selecting a main node in each
      workspace to create a relation) has to be used also when both nodes belong
      to the same tree.
    </p>
    <h4 id="swap">Swap workspaces</h4>
    <p>
      Relations are always displayed from the top to the bottom workspace. If
      you wish to see the relations in the other direction, use the <b>Swap</b>
      menu command, which swaps the top workspace with the bottom one.
    </p>
    <h4 id="attributes">Attributes</h4>
    <p>
      Just as temporal entities, temporal relations can have attributes. To see
      and/or change the values of the attributes of a particular relation, click
      on the circle placed in the middle of the relation and switch to the <a
      href=user/guide#syntax#edit>Edit</a> mode in the current workspace, as
      shown below.
    </p>
    <p>
      TODO: an example!
    </p>
    <p>
      A syntactic node and a relation node can be selected separately and
      simultaneously. The selected relation has priority over the selected node
      when it comes to the display of the attributes in the context window.

      Since modifications at the temporal relations level are not propagated
      back to the lower levels of annotation, it is most likely that no
      modifications on the syntactic or temporal nodes will occur when working
      on the temporal relations annotation.
    </p>
    <h4 id="anchor">Anchoring</h4>
    <p>
      The attributes related to temporal relations are typed, just as the
      attributes related to temporal entities. Some of these attributes
      represent anchors (i.e., references to other tree nodes), which can be
      created similarly as in <a href=user/guide#entities#anchor>the case of
      entities</a>.
    </p>
    <h4 id="delete">Delete</h4>
    <p>
      To delete the selected relation, you can use the <b>d</b> keyboard
      shortcut (a shortcut for the <a href=user/guide#syntax#delete>Delete</a>
      menu command, also available in the <em>Syntax</em> menu section).
    </p>
    <p>
      Syntactic/Temporal entity nodes and relation nodes can be selected
      separately and simultaneously. Note that, if both a relation <em>and</em>
      a node in the current window are selected, the relation will be removed
      first by the <b>Delete</b> command.
    </p>
    <h4 id="switch">Switching</h4>
    <p>
      When you create a relation linking node A with node B, a circle will
      appear on the left of both nodes, as shown below. 
    </p>
    <div class="row">
      <div class="col-sm-3"/>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/temporal/circles.png" alt="Circles" style="width:100%">
          <figcaption>Circles on the left of the selected node inform that
          incoming and outgoing relations linked with this node
          exist.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-3"/>
    </div>
    <ul>
      <li>The top-left circle tells that there is at least one relation incoming
      to the node.</li>
      <li>The bottom-left circle tells that there is at least one relation
      outgoing from the node.</li>
    </ul>
    <p>
      The circles not only tell about the incoming and outgoing relations linking
      the given node, but also serve to switch between the individual relations.
    </p>
    <ul>
      <li>The <em>bottom-left</em> circle is only clickable in the top
      workspace. It allows to switch between the relations <em>outgoing</em>
      from the node.</li>
      <li>The <em>top-left</em> circle is only clickable in the bottom
      workspace. It allows to switch between the relations <em>incoming</em> to
      the node.</li>
    </ul>
  </div>
</div>