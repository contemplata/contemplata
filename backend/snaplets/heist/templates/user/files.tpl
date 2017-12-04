<apply template="default">

  <bind tag="fileSection">

    <div class="panel panel-default">
      <div class="panel-heading">With <b><access/></b> access</div>
      <div class="panel-body">
  
        <ul class="nav nav-tabs">
          <li role="presentation" class="active"><a href="#touched${access}" data-toggle="tab"><b>In progress</b></a></li>
          <li role="presentation"><a href="#new${access}" data-toggle="tab"><b>Waiting</b></a></li>
          <li role="presentation"><a href="#done${access}" data-toggle="tab"><b>Done</b></a></li>
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
            <th>Version</th>
            <th>Tokens</th>
            <actions/>
          </bind>
          <div class="tab-pane active" id="touched${access}">
            <bind tag="panelBody"><touchedList/></bind>
            <fileTable/>
          </div>
      
          <bind tag="fileTableCols">
            <th>File name</th>
            <th>Version</th>
            <th>Tokens</th>
          </bind>
          <div class="tab-pane" id="new${access}">
            <bind tag="panelBody"><newList/></bind>
            <fileTable/>
          </div>
          <div class="tab-pane" id="done${access}">
            <bind tag="panelBody"><doneList/></bind>
            <fileTable/>
          </div>
        </div>
  
      </div>
    </div>

  </bind>

  <compareList>
    <div class="panel panel-default">
      <div class="panel-heading">Compare</div>
      <div class="panel-body">
        <compareBody/>
      </div>
    </div>
  </compareList>

  <ifNotGuest>
    <fileSection>
      <bind tag="access">Write</bind>
      <bind tag="touchedList"><touchedListWrite/></bind>
      <bind tag="newList"><newListWrite/></bind>
      <bind tag="doneList"><doneListWrite/></bind>
      <bind tag="actions">
        <th>Postpone</th>
        <th>Finish</th>
      </bind>
    </fileSection>
  </ifNotGuest>

  <fileSection>
    <bind tag="access">Read</bind>
    <bind tag="touchedList"><touchedListRead/></bind>
    <bind tag="newList"><newListRead/></bind>
    <bind tag="doneList"><doneListRead/></bind>
    <bind tag="actions"></bind>
  </fileSection>

</apply>
