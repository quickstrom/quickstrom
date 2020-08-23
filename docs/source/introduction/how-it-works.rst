How It Works
============

In Quickstrom, a tester writes *specifications* for web applications.
When *checking* a specification, the following happens:

1. Quickstrom navigates to the *origin page*, and awaits the *readyWhen*
   condition, that a specified element is present in the DOM.
2. It generates a random sequence of actions to simulate user
   interaction. Many types of actions can be generated, e.g. clicks, key
   presses, focus changes, reloads, navigations.
3. Before each new action is picked, the DOM state is checked to find
   only the actions that are possible to take. For instance, you cannot
   click buttons that are not visible. From that subset, Quickstrom
   picks the next action to take.
4. After each action has been taken, Quickstrom queries and records the
   state of relevant DOM elements. The sequence of actions takens and
   observed states is called a *behavior*.
5. The specification defines a proposition, a logical formula that
   evaluates to *true* or *false*, which is used to determine if the
   behavior is *accepted* or *rejected*.
6. When a rejected behavior is found, Quickstrom *shrinks* the sequence
   of actions to the *smallest*, *still failing*, behavior. The tester
   is presented with a minimal failing test case based on the original
   larger behavior.

You might say “This is just property-based testing!” and “I can do this
with state machine testing!” You’d be right, similar tests could be
written using a state machine model, WebDriver, and property-based
testing.

With Quickstrom, however, you don’t have to write a model that fully
specifies the behavior of your system! Instead, you describe the most
important state transitions and leave the rest unspecified. You can
gradually adopt Quickstrom and improve your specifications over time.

Furthermore, in problem domains where there’s *essential complexity*,
models tend to become as complex. It’s often hard to find a naive
implementation for your model, when your modelling a business system
with a myriad of arbitrary rules.

Now, how do you write specifications and propositions? Let’s have a look
at :doc:`specification-language`.
