<apply template="default">

  <div class="panel panel-default">
    <div class="panel-heading">Upload</div>
    <div class="panel-body">

      <div class="well"><em>
        Use this form to upload a file.  <b>Only the JSON format is supported for the moment</b>.
      </em></div>

      <dfForm id="upload-file-form">
        <div class="form-group">
          <label for="file-name">Name:</label>
          <dfInputText ref="file-name" id="file-name" class="form-control" required/>
        </div>
        <div class="form-group">
          <label for="file-level">Level:</label>
          <dfInputSelect ref="file-level" id="file-level" class="form-control" required/>
        </div>
        <div class="form-group">
          <label for="file-id">ID:</label>
          <dfInputText ref="file-id" id="file-id" class="form-control" required/>
        </div>
        <div class="form-group">
          <label for="file-path">File:</label>
          <dfInputFile ref="file-path" id="file-path" required/>
        </div>
        <dfChildErrorList class="alert alert-danger"/>
        <dfInputSubmit class="btn btn-primary" value="Upload"/>
      </dfForm>
    </div>
  </div>

</apply>
