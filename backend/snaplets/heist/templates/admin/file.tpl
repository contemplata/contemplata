<apply template="default">

  <div class="panel panel-default">
    <div class="panel-heading">Filename</div>
    <div class="panel-body">
     <fileName/>
    </div>
  </div>

  <div class="panel panel-default">
    <div class="panel-heading">Annotators</div>
    <div class="panel-body">
      <ul class="list-group">
        <currentAnnotators/>
      </ul>
      <div class="container">
        <!--dfForm id="add-anno-form" class="navbar-form navbar-left" action="/someaction"-->
        <dfForm id="add-anno-form" class="navbar-form navbar-left">
    	  <!--dfInputText ref="anno-name" class="form-control" placeholder="Username" required autofocus/-->
    	  <dfInputSelect ref="anno-name" class="form-control" required autofocus/>
          <dfInputSubmit class="btn btn-primary btn-block" value="Add"/>
        </dfForm>
      </div>
    </div>
  </div>

</apply>
