<apply template="default">

  <!--div class="well"><em>
      Below you can see the list of files in the ODIL database.
  </em></div-->

  <ul class="nav nav-tabs">
    <li role="presentation" class="active"><a href="#touched" data-toggle="tab"><b>In progress</b></a></li>
    <li role="presentation"><a href="#new" data-toggle="tab"><b>Waiting</b></a></li>
    <li role="presentation"><a href="#done" data-toggle="tab"><b>Done</b></a></li>
  </ul>

  <bind tag="fileTable">
    <table class="table table-striped">
      <thead>
        <tr><fileTableCols/></tr>
      </thead>
      <tbody>
        <panelBody/>
      </tbody>
    </table>
  </bind>

  <div class="tab-content">
    <bind tag="fileTableCols">
      <th>File name</th>
      <th>Level</th>
      <th>Access</th>
      <th>Tokens</th>
      <th>Postpone</th>
      <th>Finish</th>
    </bind>
    <div class="tab-pane active" id="touched">
      <bind tag="panelBody"><touchedList/></bind>
      <fileTable/>
    </div>

    <bind tag="fileTableCols">
      <th>File name</th>
      <th>Level</th>
      <th>Access</th>
      <th>Tokens</th>
    </bind>
    <div class="tab-pane" id="new">
      <bind tag="panelBody"><newList/></bind>
      <fileTable/>
    </div>
    <div class="tab-pane" id="done">
      <bind tag="panelBody"><doneList/></bind>
      <fileTable/>
    </div>
  </div>

</apply>
