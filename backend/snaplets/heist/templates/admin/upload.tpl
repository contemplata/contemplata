<apply template="default">

  <div class="panel panel-default">
    <div class="panel-heading">Upload</div>
    <div class="panel-body">

      <div class="well"><em>
	      Use this form to upload a new file to the annotation database. Two
        formats are available: JSON and GLOZZ corpus (`.ac`) format (see <a
        href="https://github.com/kawu/contemplata#formats">README</a> for more
        information).
      </em></div>


      <dfForm id="upload-file-form">
        <h3>File to upload</h3>
        <hr/>
        <div class="form-group">
          <!--dfLabel for="file-path">File to upload:</dfLabel-->
          <dfLabel for="file-path">Select the file in UTF-8:</dfLabel>
          <dfInputFile ref="file-path" id="file-path" required/>
        </div>
        <!--p><b>IMPORTANT:</b> The uploaded file must be encoded in UTF-8.</p-->
        <div class="form-group">
          <dfLabel for="ancor">
            <dfInputCheckbox ref="ancor" id="ancor"/>
            Upload a file in the GLOZZ format
          </dfLabel>
          <p><b>WARNING:</b> Uploading a GLOZZ file can take some time. Each
          phrase in the file is processed with a syntactic parser in this
          case.</p>
        </div>

        <h3>Annotation file</h3>
        <hr/>
        <div class="form-group">
          <dfLabel for="file-name">Base name:</dfLabel>
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

        <h3>Upload options</h3>
        <hr/>
        <div class="form-group">
          <dfLabel for="enforce">
            <dfInputCheckbox ref="enforce" id="enforce"/>
            Enforce upload if a file with the given name already exists (the
            existing file will be overwritten)
          </dfLabel>
        </div>
        <div class="form-group">
          <dfLabel for="rmPhatics">
            <dfInputCheckbox ref="rmPhatics" id="rmPhatics"/>
            Remove phatic (social obligations-related) expressions when
            uploading a GLOZZ file
          </dfLabel>
        </div>
        <successMessage/>
        <dfChildErrorList class="alert alert-danger"/>
        <dfInputSubmit class="btn btn-primary" value="Upload"/>
      </dfForm>
    </div>
  </div>

</apply>
