<apply template="default">

  <!--div class="well"><em>
  If you are a working group leader, please use the form below in order to sign in.
  </em></div-->

  <div class="container">

  <dfForm id="login-form" class="form-signin" role="form">

    <!--h2>User &rarr; Login</h2-->
    <!--h2 class="form-signin-heading">Sign In</h2-->

    <!--hr /-->

    <dfChildErrorList class="alert alert-danger" ref="" />

    <!--dfLabel class="control-label" ref="username">Login:</dfLabel-->
    <dfInputText ref="username" class="form-control" placeholder="Username" required autofocus />

    <!--dfLabel class="control-label" ref="password">Hasło:</dfLabel-->
    <dfInputPassword ref="password" class="form-control" placeholder="Password" required />

    <dfLabel class="checkbox" ref="remember">
      <dfInputCheckbox ref="remember" /> Remember me
    </dfLabel>

    <dfInputSubmit class="btn btn-lg btn-primary btn-block" value="Sign In" />

  </dfForm>

  </div> <!-- /container -->

  <!--p style="text-align: center">
    <a absHref="/register/">or create a new account</a>
  </p-->

 <!--/div-->

</apply>
