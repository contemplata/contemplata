<div class="navbar navbar-default" role="navigation">
  <div class="navbar-header">
    <!--button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse">
      <span class="sr-only">Toggle navigation</span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
    </button-->
    <a class="navbar-brand" href="">ODIL</a>
  </div>
  <div class="navbar-collapse collapse">
    <ul class="nav navbar-nav navbar-right">
      <!--li>
        <a href="contact">Contact</a>
      </li-->
      <ifAdmin>
        <li>
          <a href="admin/files">Files</a>
        </li>
        <li>
          <a href="admin/users">Users</a>
        </li>
        <!--li>
          <a href="annotation">Annotation</a>
        </li-->
      </ifAdmin>
      <ifLoggedIn>
        <ifNotGuest>
          <li>
            <a href="user/password">Password</a>
          </li>
        </ifNotGuest>
        <li>
          <a href="logout">Sign Out</a>
        </li>
      </ifLoggedIn>
      <ifLoggedOut>
        <li>
          <a href="login">Sign In</a>
        </li>
      </ifLoggedOut>
    </ul>
  </div>
</div>
