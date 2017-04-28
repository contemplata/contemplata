TIMEX
------

* Use "None" (or something similar) instead of "Null". In computer science, null
  is used to mark that a given variable has no value. None means that it has a
  value, which is none.
* Values S and R not very descriptive.  R -- relative, S -- absolute?


EVENT
-----

* POLARITY -- is it not something of a syntactic nature?


SIGNAL
------

* Example (3) -- "je suis parti après midi" -- contains a MWE. Or at least it
  makes sense to analyse it like this. Did you consider this in your analysis?
  "Après midi" could be simply analysed as a TIMEX.
* MOD = BEFORE -> AFTER?
* Using the attribute MOD makes sense, but I think that treating these two
  examples ("un peu après midi" and "après midi") as different can be well
  motivated. Put differently, I don't think "ces deux unités sont de même
  nature", as the report says.
* Now, I am going to me even more subversive. Why do we need MODIF? Syntactic
  structure should tell us what are the modifiers of a given TIMEX or SIGNAL.
* Now I am really nasty: what it the purpose of SIGNAL at all? From the semantic
  or syntactic perspective? For instance, if "après midi" is interpreted as a
  MWE, there is no signal at all. In other words, there can be easiy a relation
  between an EVENT and a TIMEX without any signal. But I'm not sure of this one
  at all, maybe SIGNAL can be really useful.
* Another example: why "au début de" is not a signal but a modifier? You could
  easily say (x) "après la guerre", and I don't see a reason why "après" should
  play a different role in example (x) than in, e.g., "après midi" (assuming
  that "après midi" is analysed compositionally).
  
  
SYNTAX
------

In general, this part is hard for me to understand, since it is not clear to me
what are the choices, which of the annotations proposed are valid, and which are
wrong...

* So which of these analyses is correct? ("Marie débuta son déjeuner à midi").
* For instance, in "Jean enseigne le lundi et Marc le mardi" (version 2), "le
  lundi" is marekd as a TIMEX, "Jean enseigne le lundi" as an EVENT, and there
  is a TLINK relation between the two. But isn't it immediately clear that this
  relation is present between the two objects, given that the TIMEX is embedded
  in the EVENT?
* Besides, this annotation does not properly account for coordination. Ah! It
  assumes an empty node? It seems to me that modern theories don't use such
  nodes. And, most likely, we are not going to get them in the result of
  parsing?
