<apply template="default">

  <div class="well"><em>
    <p>This is a guide to annotation of the ODIL files with Contemplata.</p>

    <p>
      Annotation is performed at several levels:
      <a href="user/guide#syntax">syntax</a>,
      <a href="user/guide#entities">termporal entities</a>, and
      <a href="user/guide#relations">termporal relations</a>.
      A section dedicated to each of these levels can be found below.

      Besides, some <a href="user/guide#preprocessing">preprocessing</a>-related
      steps (merging speech turns, modifying tokenization, etc.) might be
      necessary before syntactic annotation can be started.
    </p>
  </em></div>

  <ul class="nav nav-tabs">
    <li role="presentation" class="active">
      <a href="user/guide#general"><b>General information</b></a>
    </li>
    <li role="presentation">
      <a href="user/guide#preprocessing"><b>Preprocessing</b></a>
    </li>
    <li role="presentation">
      <a href="user/guide#syntax"><b>Syntax</b></a>
    </li>
    <li role="presentation">
      <a href="user/guide#entities"><b>Temporal entities</b></a>
    </li>
    <li role="presentation">
      <a href="user/guide#relations"><b>Temporal relations</b></a>
    </li>
  </ul>

  <div class="tab-content">
    <div class="tab-pane active" id="general">
      <apply template="guide/general"/>
    </div>
    <div class="tab-pane" id="preprocessing">
      <apply template="guide/preprocessing"/>
    </div>
    <div class="tab-pane" id="syntax">
      <apply template="guide/syntax"/>
    </div>
    <div class="tab-pane" id="entities">
      <apply template="guide/entities"/>
    </div>
    <div class="tab-pane" id="relations">
      <apply template="guide/relations"/>
    </div>
  </div>

  <script>
    $(window).on('hashchange', function(e){
      var url = document.location.toString();
      if (url.match('#')) {
        var tabID = url.split('#')[1];
        var eleID = url.split('#')[2];
        // console.log("tabID: " + tabID.toString());
        $('.nav-tabs a[href="user/guide#' + tabID + '"]').tab('show');
        if (eleID != null) {
          // console.log("eleID: " + eleID.toString());
          document.getElementById(eleID).scrollIntoView();
        }
      }
    });
  </script>

</apply>
