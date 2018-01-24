<div class="panel panel-default" id="preprocessing">
  <!--div class="panel-heading">Preprocessing</div-->
  <div class="panel-body">

    <p>
      Each of the files in the database is, by default, preprocessed (a stage
      when social obligations-related expressions, irrelevant for temporal
      annotation, are automatically removed), tokenized, and syntactically
      analyzed with the Stanford parser.

      Therefore, when you open a file freshly added to the database, it will
      already contain syntactic trees assigned to the individual sentences, with
      the words removed during the preprocessing stage not represented in the
      trees.
    </p>
    <p>
      Nevertheless, some additional preprocessing steps may be sometimes
      required to obtain a material which can be processed with higher accuracy
      with the Stanford parser. Speech turns may need to be merged, some
      expressions which should be removed where not automatically identified,
      division into tokens might be wrong, etc. This section describes the
      operations which allow to perform the necessary modifications.
    </p>

    <!--p>
      In many cases there is no need to modify the segmentation, already
      performed correctly by the underlying syntactic parser. For the other
      situations, several segmentation-related operations are available.
    </p-->

    <h4 id="merge">Merging turns</h4>
    <p>
      Sometimes a syntactically coherent portion of speech is divided between
      several speech turns in the file, as in the following example:
    </p>
    <figure><center>
      <img src="public/img/guide/syntax/joining-turns.png" alt="Join turns" style="width:75%">
      <figcaption>An utterance divided between several speech turns</figcaption>
    </center></figure>
    <p>
      You can merge the currently selected turn with another turn by
      CTRL+clicking on the latter in the context side window. This process can
      be iterated to join more than two turns together, as shown below.
    </p>
    <p>
      <center>
        <figure>
          <img src="public/img/guide/syntax/joining-turns-result.png" alt="Result of joining turns" style="width:75%">
          <figcaption>A tree corresponding to several speech turns after their
          merge</figcaption>
        </figure>
      </center>
    </p>
    <p>
      The merged turns are all highlighted in bold in the context side window
      when the corresponding tree is in focus.
    </p>

    <h4 id="split-sentence">Split sentence</h4>
    <p>
      Just as it is possible to merge several speech turns, it is possible to
      split a given sentence in several sub-sentences. In this case, however,
      the currently annotated speech turn is not modified, it's just the
      syntactic tree which is divided into several SENT-rooted sub-trees.

      In order to perform the operation, first select the terminal nodes which
      mark the first words of the individual sub-sentences, and then run the
      <b>Split sentence</b> menu command (<b>:splitsent</b> from command line),
      as shown below. After splitting, the individual subtrees are reparsed
      automatically.
    </p>

    <p><center>
      <figure>
        <img src="public/img/guide/syntax/split-sentence.png" alt="Split sentence" style="width:75%">
        <figcaption>Marking the terminal nodes before sentence split</figcaption>
      </figure>
    </center></p>

    <p><center>
      <figure>
        <img src="public/img/guide/syntax/split-sentence-result.png" alt="Split sentence" style="width:75%">
        <figcaption>The result of sentence split</figcaption>
      </figure>
    </center></p>

    <h4>Restart annotation</h4>
    <p>
      This operation allows to restart annotation of the sentence in focus. It
      takes the raw sentence, as shown in bold in the context side window,
      applies the preprocessing procedure (i.e., removes certain irrelevant
      expressions, as explained in the annotation guide), and then applies the
      Stanford parser in order to re-tokenize it and re-analyze it
      syntactically. It may be useful, in particular, after the <a
      href=user/guide#preprocessing#merge>merging</a> operation, which results in several
      SENT-rooted syntactic trees. <b>Restart</b>, then, re-analyses the entire
      sentence as one syntactic tree.
    </p>
    <p>
      To apply the operation, use the <b>CTRL+Restart</b> menu command (i.e.,
      click on the <b>Restart</b> menu command with CTRL pressed). The operation
      is also available via <b>:restartpreproc</b> from command line.
    </p>

    <h4>Restoring tokens</h4>
    <p>
      Sometimes, it should be usefull to discard tokens which are meaningless
      for a specific annotation. By default, Contemplata pre-processes the
      sentence to automatically remove social obligations-related expressions
      such as "s'il vous plaît" `please`, "bonsoir Monsieur" `good evening sir`,
      etc. Such removed tokens appeare in grey in the context sub-window. The
      annotator should remove additional tokens and, conversely, restore the
      erroneously removed tokens.
    </p>
    <p>
      To restore a token wrongly removed during the pre-processing step, just
      CTRL+click on it in the context side window of the current workspace. You
      can also use the <b>Restart</b> menu command (<b>:restart</b> from command
      line) to restore all the tokens and re-parse the resulting sentence.
    </p>

    <h4  id="delete-tokens">Removing tokens</h4>
    <p>
      Tokens can be manually removed in the main annotation workspace by means
      of the <a href=user/guide#syntax#deltree>Remove subtree</a> operation,
      which allows to remove the selected subtree together with the tokens
      corresponding to the terminal nodes of the subtree.
    </p>

    <h4>Dummify</h4>
    <p>
      Note that the remove subtree command does not allow to remove the entire
      tree, as the underlying model considers such a tree as invalid. In certain
      situation, however, none of the tokens is relevant, as in the example
      below. Thus we adopt a convention in which a tree with a single ROOT and
      an empty terminal node represents an empty tree. You can use the
      <b>Dummify</b> menu command (<b>dummify</b> from command line) to obtain
      it.
    </p>
    <figure><center>
      <img src="public/img/guide/syntax/dummify.png" alt="Dummify" style="width:75%">
      <figcaption>A situation where the entire tree should be removed with
      <b>Dummify</b></figcaption>
    </center></figure>

    <h4>Join words</h4>
    <p>
      When the parser incorrectly splits a token into several ones, as in the
      example below, you can select the terminal nodes corresponding to the
      tokens that should be joined, and run the <b>Join words</b> menu command
      (<b>joinwords</b> from command line).
    </p>
    <figure><center>
      <img src="public/img/guide/syntax/join-words.png" alt="Join words" style="width:75%">
      <figcaption>Joining words: all the selected terminal nodes in the figure
      represent a single phone number, hence it makes sense to join them into a
      single <em>0 1 26 0 4</em> token.</figcaption>
    </center></figure>
    <p>
      <b>NOTE</b>: the <a href=user/guide#syntax#parse>Parse</a> operation is
      automatically performed after the join.
    </p>

    <h4 id="word-split">Split words</h4>
    <p>
      The inverse operation of word joining is available via the <b>Split
      word</b> menu command (<b>splitword</b> from command line), which can be
      run after the terminal node to be split has been selected.
    </p>
    <figure><center>
    </center></figure>
    <div class="row">
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/split-word-small.png" alt="Split word" style="width:100%">
          <figcaption>A token which should be split into three separate tokens:
          <em>lui</em>, <em>-</em>, and <em>même</em>
          </figcaption>
        </center></figure>
      </div>
      <div class="col-sm-6">
        <figure><center>
          <img src="public/img/guide/syntax/split-word-small-result.png" alt="Split word" style="width:100%">
          <figcaption>The result of two subsequent word splitting</figcaption>
        </center></figure>
      </div>
    </div> 
    <p>
      <b>WARNING</b>: Each terminal in a syntactic tree should have a single
      POS-labeled parent. This condition is not satisfied after word splitting.
      The lacking POS-level nodes should be manually restored.
    </p>

  </div>
</div>
