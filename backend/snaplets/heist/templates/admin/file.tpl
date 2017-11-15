<apply template="default">

  <div class="panel panel-default">
    <div class="panel-heading">General information</div>
    <div class="panel-body">
       <p>File ID: <fileName/></p>
       <p>Status: <fileStatus/></p>
    </div>
  </div>

  <div class="panel panel-default">
    <div class="panel-heading">Annotators</div>
    <div class="panel-body">

      <!--ul class="list-group">
        <currentAnnotators/>
      </ul-->

      <table class="table">
        <thead>
          <tr>
            <th>User</th>
            <th>Remove</th>
            <th>Can modify?</th>
            <!--th><a absSortHref="country">Country</a></th-->
          </tr>
        </thead>
        <tbody>
          <currentAnnotators/>
        </tbody>
      </table>

      <dfForm id="add-anno-form" class="navbar-form navbar-left">
        <!--dfInputSubmit class="btn btn-primary btn-block" value="Add"/-->
        <dfInputSubmit class="btn btn-primary" value="Add"/>
        <dfInputSelect ref="anno-name" class="form-control" required autofocus/>
      </dfForm>

    </div>
  </div>

</apply>
