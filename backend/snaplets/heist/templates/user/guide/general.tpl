<div class="panel panel-default" id="general">
  <!--div class="panel-heading">General information</div-->
  <div class="panel-body">

    <h4 id="tableau">Tableau de bord</h4>
    <p>
      You can find the list of Contemplata files at the <a
      href="user/files">files</a> sub-page (see menu). The files listed in the
      <b>Write</b> section are for you to annotate. They are divided in three
      categories. The <b>Waiting</b> and <b>In progress</b> files are the files
      waiting for annotation and the files you started annotating, respectively.
      When you finish annotating a file, click on <b>finish</b> to move it to
      the <b>Done</b> section. Files gathered there can no longer be modified.
      You can also click on <b>postpone</b> to revert the file to the waiting
      list.
    </p>

    <p>
      As for the files to which you only have the <b>Read</b> access, they are
      either for others to annotate, or left in the database for archival
      purposes. Still, they can serve you as a reference for comparison with
      your own annotations.
    </p>

    <p>
      To annotate a file, click on its name in the first column of the table.
      You can also choose to <b>select</b> it for comparison with other files
      (the corresponding link is just beside the name of the file). This will
      create a new section with the list of the files to compare, as shown
      below. Click on <b>Compare</b> to start their annotation.
    </p>
    <div class="row">
      <div class="col-sm-1"/>
      <div class="col-sm-10">
        <figure><center>
          <img src="public/img/guide/general/compare.png" alt="Edit"
          style="width:100%"> <figcaption>Comparing files: the selected files
          (<em>1AP0061:orig:_</em> and <em>1AP0061:orig:copy</em>) are shown
          in the top-level
          window.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-1"/>
    </div>

    <p>
      To each file information about its <b>version</b> is assigned. The set of
      available versions is specified in Contemplata's configuration.

      Four version values are used in the Temporal@ODIL project. An original
      (<b>orig</b>) file is a file that has been originally put in the database.
      A <b>syntax</b> file is meant for annotation on the syntactic level.
      Similarly, a <b>temporal</b> or <b>relations</b> file is meant for
      annotation on the level of temporal entities or relations, respectively.
    </p>

    <p>
      <b>NOTE</b>: Even though Contemplata allows to modify the annotations at
      the levels incompatible with the version of the file, modifications of
      lower levels when annotating higher levels are not propagated downwards.
      For instance, modifications of syntactic nature made in the
      022_00000017:temporal file will not be copied to 022_00000017:syntax, the
      syntax version of the 022_00000017 file.

      You should therefore not exploit this possibility, which is left only for
      exceptional situations.
    </p>

    <!--p>
      Regardeless of the version of the file, you are allowed to modify it at
      any level you wish. Note, however, that modifications of lower levels when
      annotating higher levels are not propagated downwards. For instance,
      modifications of syntactic nature made in the 022_00000017:temporal file
      will not be copied to 022_00000017:syntax, the syntax version of the
      022_00000017 file.
    </p-->

    <h4>Annotation mode</h4>
    <p>
      When you click on a particular file, you will be moved to the annotation
      mode. It consists of two annotation workspaces, top and bottom, each
      showing a syntactic tree assigned to (a) particular speech turn(s) in the
      file. The side window, placed on the right, can be switched between:
    </p>
    <ul>
      <li>
        the <b>Context</b>, showing the list of the speech turns in the file,
        with the turn corresponding to the tree in the main window on the left
        marked in bold,
      </li>
      <li>
        the <b>Edit</b> mode, showing the various attributes which can be
        annotated for the selected tree node, and
        <!--with the turn being annotated marked in bold,-->
      </li>
      <li>
        the <b>Messages</b> received from the server.
      </li>
    </ul>
    <div class="row">
      <div class="col-sm-2"/>
      <div class="col-sm-8">
        <figure><center>
          <img src="public/img/guide/general/workspace.png" alt="Workspace"
          style="width:100%"> <figcaption>Annotation mode, with two main (top
          and bottom) workspaces showing the syntactic trees, and two (top and
          bottom) corresponding side windows.
          </figcaption>
        </center></figure>
      </div>
      <div class="col-sm-2"/>
    </div>

    <h4>Navigation in the annotation mode</h4>
    <p>
      To navigate between the turns in the annotation mode, you can either
      click on a particular turn in the <b>Context</b> side window, or press
      <em>Page Up</em> and <em>Page Down</em> to move up and down in the
      file, respectively.
    </p>
    <p>
      Once you find the sentence of your choice, you can slide the syntactic
      tree in different directions by left-clicking anywhere in the main
      workspace and sliding the mouse, while keeping the left mouse button
      pressed.
    </p>

    <h4 id="selection">Node selection</h4>
    <p>
      There are two types of selection: <b>main selection</b> and <b>additional
      selection</b>. Additional selection allows to select several nodes at the
      same time. Some operations only take the main selection into account,
      while others will be applied over all the selected nodes (e.g., the <a
      href="user/guide#syntax#addnode">Add</a> and <a
      href="user/guide#syntax#delnode">Delete</a> commands).
    </p>
    <p>
      To (main) select a node, it is sufficient to click on it. Additional
      selection is obtained by clicking on a node with CTRL pressed. Conversely,
      a node can be removed from additional selection by CTRL+clicking on it
      again.
    </p>

    <h4>Commands</h4>
    <h5 id="menu-command"><em>Menu commands</em></h5>
    <p>
      Three types of commands are available. The menu commands are visible in
      the top-left corner of the main annotation workspace. The first two
      commands are generic -- <b>Menu</b> and <b>Save</b>, which allow to go to
      the main menu and to save the currently annotated file, respectively. Note
      that, when you annotate several files at the same time, <b>Save</b> only
      applies to the file in focus.
    </p>
    <p>
      The other menu commands are divided into three annotation levels:
      <b>Segmentation</b>, <b>Syntax</b>, and <b>Temporal</b>. You can switch to
      any of these levels by clicking on the corresponding menu item. On the
      right of the annotation levels, the commands specific to the selected
      level are shown, as shown in the figure below.
    </p>
    <div class="row">
      <div class="col-sm-2"/>
      <div class="col-sm-8">
        <figure><center>
          <img src="public/img/guide/general/menu.png" alt="Menu commands"
          style="width:100%"> <figcaption>Menu commands are shown at the top of
          the main workspace. On the right of the selected <em>Segmentation</em>
          level, the commands specific to this level -- <em>Restart</em>,
          <em>Split sentence</em>, <em>Split word</em> etc. -- are
          shown.</figcaption>
        </center></figure>
      </div>
      <div class="col-sm-2"/>
    </div>
    <p>
      The command can have two different behaviors, depending on whether CTRL is
      pressed or not. Such commands are marked in italics when the CTRL key is
      pressed. You can also place the mouse over a menu command to see the
      corresponding help message.
    </p>
    <!--p>
      The commands from the other levels can be still used via the command line
      or keyboard shortcuts, though.
    </p-->
    <h5 id="keyboard-shortcut"><em>Keyboard shortcuts</em></h5>
    <p>
      Some commands are available via keyboard shortcuts, which are typically
      indicated in the names of their corresponding menu commands as
      underlined characters. For instance, the <b><u>P</u>arse</b> command from
      the syntactic annotation level can be run with the <b>p</b> keyboard
      shortcut.
    </p>
    <h5 id="command-line"><em>Command line</em></h5>
    <p>
      The third type of commands can be run via command line. After pressing
      <b>Space</b>, you will see the list of available commands of this type at
      the bottom of the workspace. To run a particular command, just type it and
      press <b>Enter</b>. You can also use <b>Tab</b> for auto-completion.
      Besides, you don't have to type the entire command, just a prefix which
      uniquely identifies it (for instance, <b>sa</b> for <b>save</b>).
    </p>
    <div class="row">
      <div class="col-sm-2"/>
      <div class="col-sm-8">
        <figure><center>
          <img src="public/img/guide/general/command-line.png" alt="Command line" style="width:100%">
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

    <h4>Re-sizing</h4>
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
