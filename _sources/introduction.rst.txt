Introduction
============

By using Quickstrom, you will:

-  **Reduce testing effort**

   Generate thousands of test cases instead of writing them manually.
   Lower the maintenance burden of your browser testing by writing
   concise specifications with looser coupling to your implementation.

-  **Find complex bugs**

   Quickstrom simulates complex and unexpected user behavior using
   generative random testing. When the specification is violated,
   Quickstrom finds a minimal failing example.

-  **Understand your system**

   Focus on specifying your system, not on writing test cases. A
   specification lets you run Quickstrom, but can also increase your
   team’s understanding of the system.

-  **Adopt gradually**

   Quickstrom works with any web application that renders DOM elements.
   Start simple, and gradually refine your specification to increase
   coverage and confidence.

That sounds cool, but does it actually work?

Yes! Check out `The TodoMVC
Showdown <https://wickstrom.tech/programming/2020/07/02/the-todomvc-showdown-testing-with-webcheck.html>`__
case study and learn how Quickstrom (previously called *WebCheck*) found
problems in multiple implementations of TodoMVC.

How It Works
------------

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
   state of relevant DOM elements. The sequence of observed states is
   called a *behavior*.
5. The specification defines a proposition, a logical formula that
   evaluates to *true* or *false*, which is used to determine if the
   behavior is *accepted* or *rejected*.
6. When a rejected behavior is found, Quickstrom *shrinks* the sequence
   of actions to the *smallest*, *still failing*, behavior. The tester
   is presented with a minimal failing test case.

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
at the specification language.

The Quickstrom Specification Language
-------------------------------------

In Quickstrom, the behavior of a web application is specified using a
language based on PureScript. It’s a propositional temporal logic and
functional language, heavily inspired by TLA+ and LTL, most notably
adding web-specific operators.

Like in TLA+, specifications in Quickstrom are based on state machines.
A *behavior* is a finite sequence of states. A *step* is a tuple of two
successive states in a behavior. A specification describes valid
*behaviors* of a web application in terms of valid states and
transitions between states.

As in regular PureScript, every expression evaluates to a *value*. A
*proposition* is a boolean expression in a specification, evaluating to
either ``true`` or ``false``. A specification that accepts *any*
behavior could therefore be:

.. code:: haskell

   module Spec where

   proposition = true

   ... -- more definitions, explained further down

To define a useful specification, though, we need to perform *queries*
and desribe how things change over time (using *temporal operators*).

Queries
~~~~~~~

Quickstrom provides two ways of querying the DOM in your specification:

-  ``queryAll``
-  ``queryOne``

Both take a CSS selector and a record of element state specifiers, e.g.
attributes or properties that you’re interested in.

For example, the following query finds all buttons, including their text
contents and disabled flags:

.. code:: haskell

   myButtons = queryAll "button" { textContent, disabled }

The type of the above expression is:

::

   Array { textContent :: String, disabled :: Boolean }

You can use regular PureScript function to map, filter, or whatever
you’d like, on the array of button records.

In contrast to ``queryAll`` returning an ``Array``, ``queryOne`` returns
a ``Maybe``.

Temporal Operators
~~~~~~~~~~~~~~~~~~

In Quickstrom specifications, there are two temporal operators:

-  ``next :: forall a. a -> a``
-  ``always :: forall a. a -> a``

They change the *modality* of the sub-expression, i.e. in what state of
the recorded behavior it is evaluated.

Always
^^^^^^

Let’s say we have the following proposition:

.. code:: haskell

   proposition = always (title == Just "Home")

   title = map _.textContent (queryOne "h1" { textContent })

In every observed state the sub-expression must evaluate to ``true`` for
the proposition to be true. In this case, the text content of the ``h1``
must always be “Home”.

Next
^^^^

Let’s modify the previous proposition to describe a state change:

.. code:: haskell

   proposition = always (goToAbout || goToContact)

   goToAbout = title == Just "Home" && next title == "About"

   goToContact = title == Just "Home" && next title == "Contact"

   title = map _.textContent (queryOne "h1" { textContent })

We’re now saying that it’s always the case that one or another *action*
is taken. An action is a boolean expression that uses ``next`` to
describe the current and the next state, i.e. a state transition.

The ``goToAbout`` and ``goToContact`` actions specifies how the title of
the page changes, and the proposition thus describes the system as a
state machine.

Example: Record Player
~~~~~~~~~~~~~~~~~~~~~~

The following specifies a record player, featuring a button that toggles
between the paused and playing states.

It waits for a DOM element matching the CSS selector .record-player
before taking any action.

.. code:: haskell

   readyWhen = ".record-player"

This helper definition finds an optional text for the play/pause button.

.. code:: haskell

   buttonText =
     map _.textContent (queryOne ".play-pause" { textContent })

Quickstrom generates click actions for all clickable elements.

.. code:: haskell

   actions = clicks

The proposition describes the correct behavior of the web application.
Here we start in the paused state, and a valid transition is either play
or pause.

.. code:: haskell

   proposition =
     let playing = buttonText == Just "Pause"
         paused = buttonText == Just "Play"
         play = paused && next playing
         pause = playing && next paused
     in paused && always (play || pause)

Now, let’s run Quickstrom with a broken implementation of the record
player. We get a minimal behavior that violates the specification:

.. code:: shell

   $ quickstrom check RecordPlayer.spec.purs record-player.html
   Running test with size: 10
   Test failed. Shrinking...
   1. State
     • .play-pause
         - textContent = "Play"
   2. click button[0]
   3. State
     • .play-pause
         - textContent = "Pause"
   4. click button[0]
   5. State
     • .play-pause
         - textContent = "undefined"

Although not highlighted, the last item with the ``undefined`` text is
where we have our problem. Looks like pausing broke the record player!

Going Further
-------------

More documentation on how to use Quickstrom will be written. For now,
head over to the `example specifications and
webapps <https://github.com/quickstrom/quickstrom/tree/main/specs>`__ to
study further.

When you’re ready to run Quickstrom on your own, see
:doc:`installation` and :doc:`running`.
