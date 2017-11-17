<apply template="default">

    <div class="well"><em>
	You can change your password on this subpage.  <b>Don't use your
top-secret password, though</b>. Passwords are stored encrypted on the server,
but the connection is not secured.
    </em></div>

    <div class="panel panel-default">
      <div class="panel-heading">Change password</div>
      <div class="panel-body">

        <dfForm id="change-password-form">
        
          <dfChildErrorList class="alert alert-danger"/>
          <onSuccess/>
        
          <div class="form-group">
            <dfLabel for="user-oldpass">Current password</dfLabel>
            <dfInputPassword class="form-control" id="user-oldpass" ref="user-oldpass" placeholder="Enter current password" required autofocus/>
          </div>
        
          <div class="form-group">
            <dfLabel for="user-newpass1">New password</dfLabel>
            <dfInputPassword id="user-newpass1" ref="user-newpass1" class="form-control" placeholder="Enter new password" required/>
          </div>
    
          <div class="form-group">
            <dfLabel for="user-newpass2">Repeat new password</dfLabel>
            <dfInputPassword id="user-newpass2" ref="user-newpass2" class="form-control" placeholder="Enter again new password" required/>
          </div>
        
          <dfInputSubmit class="btn btn-primary" value="Submit"/>
        
        </dfForm>

      </div>
    </div>

</apply>
