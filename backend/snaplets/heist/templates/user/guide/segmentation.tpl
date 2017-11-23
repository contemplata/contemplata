<div class="panel panel-default" id="segmentation">
  <div class="panel-heading">Segmentation</div>
  <div class="panel-body">

    <p>
      In many cases there is no need to modify the segmentation, already
      performed correctly by the underlying syntactic parser. For the other
      situation, several segmentation-related operations are available.
    </p>

    <h4>Merging turns</h4>

    <p>
      Sometimes a syntactically coherent portion of speech is devided between
      several speech turns in the file, as in the following example:
    </p>

    <figure><center>
      <img src="/public/img/guide/syntax/joining-turns.png" alt="Join turns" style="width:75%">
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
          <img src="/public/img/guide/syntax/joining-turns-result.png" alt="Result of joining turns" style="width:75%">
          <figcaption>A tree corresponding to several speech turns after their
          merge</figcaption>
        </figure>
      </center>
    </p>

    <h4>Restart with preprocessing</h4>
    <p>
      To syntactically annotate the given sentence from scratch, use the
      <b>CTRL+Restart</b> menu command (<b>:restartpreproc</b> from command
      line). It restores all the tokens, performs pre-processing, and uses the
      parser to analyse the sentence.

      It may be useful, in particular, after the merging operation, which
      results in several SENT-rooted syntactic trees. Restart, then,
      re-analyses the entire sentence as one syntactic tree.
    </p>

    <h4>Restoring tokens</h4>
    <p>
      To restore a token wrongly removed during the pre-processing step (such
      tokens are marked in grey), just CTRL+click on it in the context side
      window of the current workspace. You can also use the <b>Restart</b>
      menu command (<b>:restart</b> from command line) to restore all the
      tokens and re-parse the resulting sentence.
    </p>

    <h4>Removing tokens</h4>
    <p>
      Tokens can be manually removed in the main annotation workspace. Select
      the root of the tree to be removed and use the <b>CTRL+d</b> keyboard
      shortcut, the <b>CTRL+Delete</b> menu command (currently in the
      <em>Syntax</em> annotation mode), or <b>delnode</b> from command line.
      As a result, all the tokens corresponding to the terminal nodes in the
      tree will be removed.

      Note that a subtree can be only removed if the resulting syntactic tree
      is well structured. The following example shows a situation where
      performing the command is not allowed, becuase it would lead to a tree
      with a non-terminal (VN) leaf. Removing the VN node itself with its
      subtree is, on the other hand, perfectly fine.
    </p>
    <figure><center>
      <img src="/public/img/guide/syntax/remove-subtree.png" alt="Remove subtree" style="width:75%">
      <figcaption>A context in which removing the subtree of the selected node
      is not allowed</figcaption>
    </center></figure>

    <h4>Split sentence</h4>
    <p>
      Just as it is possible to merge several speech turns, it is possible to
      split a given sentence in several sub-sentences. In this case, however,
      the currently annotated speech turn is not modified, it's just the
      syntactic tree which is divided into several SENT-rooted sub-trees.

      In order to perform the operation, first select the terminal nodes which
      mark the first words of the individual sub-sentences, and then run the
      <b>Split sentence</b> menu command (<b>:splitsent</b> from command
      line), as shown below.
    </p>

    <p><center>
      <figure>
        <img src="/public/img/guide/syntax/split-sentence.png" alt="Split sentence" style="width:75%">
        <figcaption>Marking the terminal nodes before sentence split</figcaption>
      </figure>
    </center></p>

    <p><center>
      <figure>
        <img src="/public/img/guide/syntax/split-sentence-result.png" alt="Split sentence" style="width:75%">
        <figcaption>The result of sentence split</figcaption>
      </figure>
    </center></p>
      
    <h4>Join words</h4>
    <p>
      When the parser incorrectly splits a token into several ones, as in the
      example below, you can select the terminal nodes corresponding to the
      tokens that should be joined, and run the <b>Join words</b> menu command
      (<b>joinwords</b> from command line).
    </p>
    <figure><center>
      <img src="/public/img/guide/syntax/join-words.png" alt="Join words" style="width:75%">
      <figcaption>Joining words</figcaption>
    </center></figure>

    <h4>Split words</h4>
    <p>
      The inverse operation of word splitting is available via the <b>Split
      word</b> menu command (<b>splitword</b> from command line), which can be
      run after the terminal node to be split has been selected.
    </p>
    <figure><center>
      <img src="/public/img/guide/syntax/split-word.png" alt="Split word" style="width:75%">
      <figcaption>A token which should be split into three separate tokens:
      <em>lui</em>, <em>-</em>, and <em>même</em>
      </figcaption>
    </center></figure>

    <h4>Dummify</h4>
    <p>
      Note that the delete tree command does not allow to remove the entire
      tree, as the underlying model considers such a tree as invalid. In
      certain situation, however, none of the tokens is relevant, as in the
      example below. Thus we adopt a convention in which a tree with a single
      ROOT and an empty terminal node represents an empty tree. You can use
      the <b>Dummify</b> menu command (<b>dummify</b> from command line) to
      obtain it.
    </p>
    <figure><center>
      <img src="/public/img/guide/syntax/dummify.png" alt="Dummify" style="width:75%">
      <figcaption>A situation where the entire tree should be removed with
      <b>Dummify</b></figcaption>
    </center></figure>
  </div>
</div>