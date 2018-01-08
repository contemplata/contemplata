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

      <annoForm id="add-anno-form" class="navbar-form navbar-left">
        <!--annoInputSubmit class="btn btn-primary btn-block" value="Add"/-->
        <annoInputSubmit class="btn btn-primary" name="add_button" value="Add"/>
        <annoInputSelect ref="anno-name" class="form-control" required autofocus/>
      </annoForm>

    </div>
  </div>

  <div class="panel panel-default">
    <div class="panel-heading">Copy</div>
    <div class="panel-body">

      <div class="well"><em>
        Use this form to create and store in the database a copy of the file.
      </em></div>

      <!--copyForm id="copy-file-form" class="navbar-form navbar-left"-->
      <copyForm id="copy-file-form">
        <div class="form-group">
          <label for="file-name">Name:</label>
          <!--input type="email" class="form-control" id="email"-->
          <copyInputText ref="file-name" id="file-name" class="form-control" required/>
        </div>
        <div class="form-group">
          <label for="file-level">Level:</label>
          <copyInputSelect ref="file-level" id="file-level" class="form-control" required/>
        </div>

        <div class="form-group">
          <label for="file-id">ID:</label>
          <copyInputText ref="file-id" id="file-id" class="form-control" required/>
        </div>

        <copyInputSubmit class="btn btn-primary" name="copy_button" value="Create copy"/>
      </copyForm>
    </div>
  </div>

</apply>
