The Specification Language
==========================

In Quickstrom, the behavior of a web application is described in a
specification language. It’s a propositional temporal logic and functional
language, heavily inspired by TLA+ and LTL, most notably adding web-specific
operators. The specification language of Quickstrom is based on `PureScript
<https://www.purescript.org/>`__.

Like in TLA+, specifications in Quickstrom are based on state machines.
A *behavior* is a finite sequence of states. A *step* is a tuple of two
successive states in a behavior. A specification describes valid
*behaviors* of a web application in terms of valid states and
transitions between states.

As in regular PureScript, every expression evaluates to a *value*. A
*proposition* is a boolean expression in a specification, evaluating to
either ``true`` or ``false``. A specification that accepts *any*
behavior could therefore be:

.. code-block:: haskell

   module Spec where

   proposition = true

   ... -- more definitions, explained further down

To define a useful specification, though, we need to perform *queries*
and describe how things change over time (using *temporal operators*).

Queries
-------

Quickstrom provides two ways of querying the DOM in your specification:

-  ``queryAll``
-  ``queryOne``

Both take a CSS selector and a record of element state specifiers, e.g.
attributes or properties that you’re interested in.

For example, the following query finds all buttons, including their text
contents and disabled flags:

.. code-block:: haskell

   myButtons = queryAll "button" { textContent, disabled }

The type of the above expression is:

::

   Array { textContent :: String, disabled :: Boolean }

You can use regular PureScript function to map, filter, or whatever
you’d like, on the array of button records.

In contrast to ``queryAll`` returning an ``Array``, ``queryOne`` returns
a ``Maybe``.

Temporal Operators
------------------

In Quickstrom specifications, there are three core temporal operators:

-  ``next :: forall a. a -> a``
-  ``always :: Boolean -> Boolean``
-  ``until :: Boolean -> Boolean -> Boolean``

They change the *modality* of the sub-expression, i.e. in what state of
the recorded behavior it is evaluated.

There are also utility functions built on top of the temporal operators:

- ``unchanged :: Eq a => a -> Boolean``

Let's go through the operators and utility functions provided by
Quickstrom!

Always
~~~~~~

Let’s say we have the following proposition:

.. code-block:: haskell

   proposition = always (title == Just "Home")

   title = map _.textContent (queryOne "h1" { textContent })

In every observed state the sub-expression must evaluate to ``true`` for
the proposition to be true. In this case, the text content of the ``h1``
must always be “Home”.

Until
~~~~~

Until takes two parmeters: the prerequisite condition and the final condition.
The prerequisite must hold ``true`` in all states until the final condition is
``true``.

.. code-block:: haskell

   proposition = until (loading == Just "loading...") (title == Just "Home")

   loading = map _.textContent (queryOne "loading" { textContent })
   title = map _.textContent (queryOne "h1" { textContent })

In this case, we presumably load the “Home” text from somewhere else,
so we wait until the loading is done, and then assert that the title must be
set accordingly.

Next
~~~~

Let’s modify the previous proposition to describe a state change:

.. code-block:: haskell

   proposition = always (goToAbout || goToContact || goHome)

   goToAbout = title == Just "Home" && next title == Just "About"

   goToContact = title == Just "Home" && next title == Just "Contact"

   goHome = title /= Just "Home" && next title == Just "Home"

   title = map _.textContent (queryOne "h1" { textContent })

We’re now saying that it’s always the case that one or another state
transition occurs. An state transition is represented as a boolean expression,
using queries and ``next`` to describe the current and the next state.

The ``goToAbout``, ``goToContact``, and ``goHome`` transitions specify how the
title of the page changes, and the ``proposition`` thus describes the system
as a state machine. It can be visualized as follows:

.. graphviz::

   digraph foo {
     graph [ dpi = 300 ];
     splines=true;
     esep=10;
     size="5";
     rankdir=LR;
     edge [ fontname = "Open Sans" ];
     node [ fontname = "Open Sans Bold", margin = "0.5,0.5" ];

     Home -> About [ label = "goToAbout" ];
     Home -> Contact [ label = "goToContact" ];
     About -> Home [ label = "goHome" ];
     Contact -> Home [ label = "goHome" ];
   }

Unchanged
~~~~~~~~~

In addition to the core temporal operators, the ``unchanged`` operator
is a utility for stating that something does *not* change:

.. code-block:: haskell

   unchanged :: forall a. Eq a => a -> Boolean
   unchanged x = x == next x

It's useful when expressing state transitions, specifying that a
certain queried value should be the same both before and after a
particular transition.

For instance, let's say we have a bunch of top-level definitions, all
based on DOM queries, describing a user profile:

.. code-block:: haskell

   userName :: String
   userName = ...

   userProfileUrl :: String
   userProfileUrl = ...

We can say the user profile information should not change in a
transition ``t`` by passing an array of those values:

.. code-block:: haskell

   t = unchanged [userName, userProfileUrl]
       && ... -- actual changes in transition

Actions
-------

We must instruct Quickstrom what actions it should try. The ``actions``
definition in a specification module is where you list possible actions.

.. code-block:: haskell

   actions :: Actions
   actions = [ action1, action2, ... ]

It's an array of values, where each value describes an action or a fixed
sequence of actions. Each action also carries a weight, which specifies the
intended probability of the action being picked, relative to the other
actions.

The default weight is ``1``. To override it, use the ``weighted`` function:

.. code-block:: haskell

   click "#important-action" `weighted` 10

To illustrate, in the following array of actions, the probability of ``a1``
being picked is 40%, while the others are at 20% each. This is assuming the
action (or the first action in each sequence) is *possible* at each point a
sequence is being picked.

.. code-block:: haskell

   actions = [
       a1 `weighted` 2,
       a2,
       a3,
       a4
     ]

Action Sequences
~~~~~~~~~~~~~~~~

An action sequence is either a single action or a fixed sequence of actions.
Here's a simple sequence:

.. code-block:: haskell

   backAndForth = click "#back" `followedBy` click "#forward"

A sequence of actions is always performed in its entirety when picked, as
long as the first action in the sequence is considered possible by the test
runner.

Actions
~~~~~~~

The available actions are provided in the Quickstrom library:

* ``focus``
* ``keyPress``
* ``enterText``
* ``click``
* ``clear``
* ``await``
* ``awaitWithTimeoutSecs``
* ``navigate``
* ``refresh``

Along with those functions, there are some aliases for common actions. For
instance, here's the definition of ``foci``:

.. code-block:: haskell

   -- | Generate focus actions on common focusable elements.
   foci :: Actions
   foci = 
      [ focus "input"
      , focus "textarea"
      ]

More actions and aliases should be introduced as Quickstrom evolves.

Example
~~~~~~~

As an example of composing actions and sequences of actions, here's a
collection of actions that try to log in or to click a buy button:

.. code-block:: haskell

   actions = 
      [ focus "input[type=password]"
          `followedBy` enterText "$ecr3tz"
          `followedBy` click "input[type=submit][name=log-in]"
      , click "input[type=submit][name=buy]"
      ]

.. note::

   When specifying complex web applications, one must often carefully pick
   selectors, actions, and weights, to effectively test enough within
   a reasonable time. Aliases like ``clicks`` and ``foci`` might not work
   well in such situations.
