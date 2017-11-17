<apply template="default">

    <div class="well"><em>
        Below you can see the list of users in the ODIL database.
    </em></div>

    <div class="panel panel-default">
      <div class="panel-heading">Existing users</div>
      <div class="panel-body">
        <ul class="list-group">
          <userList/>
        </ul>
      </div>
    </div>

    <div class="panel panel-default">
      <div class="panel-heading">New user</div>
      <div class="panel-body">
        <dfForm id="add-user-form">
    
          <dfChildErrorList class="alert alert-danger"/>
    
          <div class="form-group">
            <dfLabel for="user-name">Login</dfLabel>
            <dfInputText class="form-control" id="user-name" ref="user-name" placeholder="Enter login"/>
          </div>
    
          <div class="form-group">
            <dfLabel for="user-pass">Password</dfLabel>
            <dfInputPassword id="user-pass" ref="user-pass" class="form-control" placeholder="Password" required/>
          </div>
    
          <dfInputSubmit class="btn btn-primary" value="Add new user"/>
    
        </dfForm>
      </div>
    </div>

</apply>
