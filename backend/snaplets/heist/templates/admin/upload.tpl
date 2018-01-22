<apply template="default">

  <div class="panel panel-default">
    <div class="panel-heading">Upload</div>
    <div class="panel-body">

      <div class="well"><em>
        Use this form to upload a file.  <b>Only the JSON format is supported for the moment</b>.
      </em></div>


      <dfForm id="upload-file-form">
        <div class="form-group">
          <dfLabel for="file-name">Name:</dfLabel>
          <dfInputText ref="file-name" id="file-name" class="form-control" required/>
        </div>
        <div class="form-group">
          <dfLabel for="file-level">Level:</dfLabel>
          <dfInputSelect ref="file-level" id="file-level" class="form-control" required/>
        </div>
        <div class="form-group">
          <dfLabel for="file-id">ID:</dfLabel>
          <dfInputText ref="file-id" id="file-id" class="form-control" required/>
        </div>
        <div class="form-group">
          <dfLabel for="enforce">
            <dfInputCheckbox ref="enforce" id="enforce"/>
            Enforce upload if a file with the given name already exists
          </dfLabel>
        </div>
        <div class="form-group">
          <dfLabel for="ancor">
            <dfInputCheckbox ref="ancor" id="ancor"/>
            Upload a file in the ANCOR format
          </dfLabel>
        </div>
        <div class="form-group">
          <dfLabel for="rmPhatics">
            <dfInputCheckbox ref="rmPhatics" id="rmPhatics"/>
            Remove phatic (social obligations-related) expressions when uploading an ANCOR file 
          </dfLabel>
        </div>
        <div class="form-group">
          <dfLabel for="file-path">File:</dfLabel>
          <dfInputFile ref="file-path" id="file-path" required/>
        </div>
        <successMessage/>
        <dfChildErrorList class="alert alert-danger"/>
        <dfInputSubmit class="btn btn-primary" value="Upload"/>
      </dfForm>
    </div>
  </div>

</apply>
