<apply template="default">

  <!--div class="well"><em>
      Below you can see the list of files in the ODIL database.
  </em></div-->

  <ul class="nav nav-tabs">
    <li role="presentation" class="active"><a href="#touched" data-toggle="tab">In progress</a></li>
    <li role="presentation"><a href="#new" data-toggle="tab">New</a></li>
    <li role="presentation"><a href="#done" data-toggle="tab">Done</a></li>
  </ul>

  <bind tag="fileSection">
    <!--h3><panelHeader/></h3-->
    <table class="table table-striped">
      <thead>
        <tr>
          <th>File name</th>
          <th>Annotation level</th>
          <th>Tokens</th>
        </tr>
      </thead>
      <tbody>
        <panelBody/>
      </tbody>
    </table>
  </bind>

  <div class="tab-content">
    <div class="tab-pane active" id="touched">
      <bind tag="panelHeader">Currently annotated files</bind>
      <bind tag="panelBody"><touchedList/></bind>
      <fileSection/>
    </div>
    <div class="tab-pane" id="new">
      <bind tag="panelHeader">New files</bind>
      <bind tag="panelBody"><newList/></bind>
      <fileSection/>
    </div>
    <div class="tab-pane" id="done">
      <bind tag="panelHeader">Finished files</bind>
      <bind tag="panelBody"><doneList/></bind>
      <fileSection/>
    </div>
  </div>

</apply>
