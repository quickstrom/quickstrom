How It Works
============

In Quickstrom, a tester writes *specifications* for web applications.
When *checking* a specification, the following happens:

1. Quickstrom navigates to the *origin page*, and awaits the initial
   event (e.g. that the page has loaded).
2. Based on the current DOM state, it generates a random action to
   simulate user interaction. Many types of actions can be generated,
   e.g. clicks, key presses, focus changes, reloads, navigations.

   Only actions with their *preconditions* met can be chosen. For
   instance, you cannot click buttons that are currently invisible.
3. After an action has been taken, Quickstrom queries and records the
   state of relevant DOM elements.

   If the action takes specifies a *timeout*, Quickstrom waits the
   given duration for any asynchronous events to occur, e.g. that
   a DOM node changed.

   Steps 2 and 3 are repeated as long as the specification requires
   more states and actions. The recorded sequence of states and actions
   is called a *trace*.
4. The specification defines one or more proposition to be checked. A
   proposition is a logical formula, which is used to determine if the
   trace is *successful* or *failed*. As soon as a proposition has a
   definitive answer, the check stops.

Now, how do you write specifications and propositions? Let’s have a look
at :doc:`specification-language`.
