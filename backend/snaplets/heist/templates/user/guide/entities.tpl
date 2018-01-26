<div class="panel panel-default" id="entities">
  <!--div class="panel-heading">Temporal entities</div-->
  <div class="panel-body">

    <h4>Marking events, signals, and temporal expressions</h4>
    <p>
      To annotate the selected node as an event, use the <b>E<u>v</u>ent</b> <a
      href="user/guide#general#menu-command"> menu command</a>.

      Similarly, the <b><u>S</u>ignal</b> menu command allows to annotate
      signals, and the <b><u>T</u>imex</B> menu command -- temporal expressions.

      Recall that to each command a <a
      href="user/guide#general#keyboard-shortcut">keyboard shortcut</a> is also
      assigned, underlined in the corresponding menu item (e.g., 'v' for
      'Event').
    </p>
    <p>
      Note that, after you mark a particular node as a signal/event/timex, a set
      of dedicated attributes becomes available in the <b>Edit</b> side window,
      as shown in the example below.
    </p>
    <div class="row">
      <div class="col-sm-3"/>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/temporal/attributes.png" alt="Attributes" style="width:100%">
          <figcaption>Attributes available for annotation for the selected event.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-3"/>
    </div>
    <p>
      To unmark a given node as an event/signal/timex, just select the node and
      use the corresponding menu command (or keyboard shortcut) again.
    </p>

    <h4 id="anchor">Anchoring</h4>
    <p>
      Most of the attributes have a prescribed, closed set of possible values.
      These values can be changer via the <a
      href="user/guide#syntax#edit">Edit</a> side window, which can be quickly
      reached via the <b>e</b> keyboard shortcut.

      However, some temporal entities have attributes which perform the function
      of relations between the selected node and another node. For instance,
      each Timex can be anchored at another temporal entity, as shown below.
    </p>
    <p>
      TODO: need a good example!
    </p>
    <p>
      Two ways of setting the anchor of the selected node are available:
    </p>
    <ul>
      <li>Select a single additional node in the same tree and click the
      <em>Create</em> button next to the anchor attribute.</li>
      <li>Select a main node in the alternative workspace and click
      <em>Create</em>.</li>
    </ul>
    <p>
      Note that there can be other attributes of the anchor type. For instance,
      the <em>Begin</em> and <em>End</em> attributes of a Timex (when its type
      is set to <em>Duration</em>) are of the anchor type as well.
    </p>
    <p>
      Currently, the tool requires that the node being anchored is typed (e.g.,
      is an Event). Otherwise, the anchoring action is rejected.
    </p>

  </div>
</div>
