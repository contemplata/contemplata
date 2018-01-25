<div class="panel panel-default" id="relations">
  <!--div class="panel-heading">Temporal relations</div-->
  <div class="panel-body">
    <h4 id="types">Types</h4>
    <p>
      Contemplata allows to draw typed relations between tree nodes. In the
      Temporal@ODIL project, two (TODO: four) types of relations can be created:
      SLink and TLink. To each type the corresponding command can be found in
      the menu (the <b>Temporal</b> section).
    </p>
    <h4 id="create">Create</h4>
    <p>
      To create a new relation, select a main node in each of the two workspaces
      and use one of the related commands. The new relation will lead from the
      node selected in the top window to the one in the bottom window.
    </p>
    <h4 id="swap">Swap workspaces</h4>
    <p>
      Relations are always drawn from the top to the bottom workspace. If you
      wish to see the relations in the other direction, use the <b>Swap</b> menu
      command, which swaps the top workspace with the bottom one.
    </p>
    <p>
      While the relations could be shown in the other direction as well, we the
      one-directional solution should allow to avoid unnecessary clutter, when
      many relations are present between the two trees in focus.
    </p>
    <h4 id="attributes">Attributes</h4>
    <p>
      Just as temporal entities, temporal relations can have attributes. To see
      and/or change the values of the attributes of a particular relation, click
      on the circle placed in the middle of the relation and switch to the
      <em>Edit</em> mode in the current workspace (or via the <a
      href=user/guide#syntax#edit>Edit</a> command), as shown below.
    </p>
    <p>
      TODO: an example!
    </p>
    <h4 id="delete">Delete</h4>
    <p>
      To delete the selected relation, you can use the <b>d</b> keyboard
      shortcut (a shortcut for the <a href=user/guide#syntax#delete>Delete</a>
      menu command, also available in the <em>Syntax</em> menu section).
    </p>
    <p>
      Note that, if both a relation <em>and</em> a node in the current window
      are selected, the relation will be removed first in case of the
      <b>Delete</b> command. Also, the selected relation has a priority over the
      selected node when it comes to the display of the attributes in the
      context window.
    </p>
  </div>
</div>