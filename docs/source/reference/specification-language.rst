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

In Quickstrom specifications, there are two temporal operators:

-  ``next :: forall a. a -> a``
-  ``always :: forall a. a -> a``

They change the *modality* of the sub-expression, i.e. in what state of
the recorded behavior it is evaluated.

Always
~~~~~~

Let’s say we have the following proposition:

.. code-block:: haskell

   proposition = always (title == Just "Home")

   title = map _.textContent (queryOne "h1" { textContent })

In every observed state the sub-expression must evaluate to ``true`` for
the proposition to be true. In this case, the text content of the ``h1``
must always be “Home”.

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

Actions
-------

We must instruct Quickstrom what actions it should try. The ``actions``
definition in a specification module has the following type:

.. code-block:: haskell

   Array (Tuple Int Action)

It's an array of pairs, or tuples, where each pair holds a weight and an
action specifier. The weight specifies the intended probability of the action
being picked, relative to the other actions.

To illustrate, in the following array of actions, the probability of ``a1``
being picked is 40%, while the others are at 20% each. This is assuming all
actions are *possible* at each point an action is being picked.

.. code-block::

   actions = [
       Tuple 2 a1,
       Tuple 1 a2,
       Tuple 1 a3,
       Tuple 1 a4
     ]

The ``Action`` data type is defined in the Quickstrom library, along with
some aliases for common actions. For instance, here's the definition of
``foci``:

.. code-block:: haskell

   -- | Generate focus actions on common focusable elements.
   foci :: Actions
   foci = [ Tuple 1 (Focus "input"), Tuple 1 (Focus "textarea") ]

More action constructors and aliases should be introduced as Quickstrom
evolves.

.. note::

   When specifying complex web applications, one must often carefully pick
   selectors, actions, and weights, to effectively test enough within
   a reasonable time. Aliases like ``clicks`` and ``foci`` might not work
   well in such situations.