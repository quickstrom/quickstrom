The Specification Language
==========================

In Quickstrom, the intended behavior of a web application is described
in a specification language called *Specstrom*. It’s a propositional
temporal logic with a functional expression language. Syntax-wise we
try to keep the language close to JavaScript, although semantically
it's quite different.

This document is a high-level and informal walkthrough of the
language. Have a look at the paper `Quickstrom: Property-based
Acceptance Testing with LTL Specifications
<https://arxiv.org/pdf/2203.11532.pdf>`__ if you're interested in a
more detailed description of the underlying temporal logic, called
QuickLTL.

Formulae
--------

The top level construct in Quickstrom are linear temporal logic
formulae, where you can describe how the state of the web application
should be now and in the future.

For example, if we want ``x`` to be true in the current state, and
``y`` in the next state, we can express it like this:

.. code-block:: js

   x && next y

If we want ``x`` to be true zero or more states until ``y`` becomes true,
we say:

.. code-block:: js

   x until y

These examples are formulae, where ``x`` and ``y`` are free variables,
and ``next`` and ``until`` are temporal operators.

Expressions
-----------

At a lower level we have an expression language, which is a functional
language. We can do basic stuff like arithmetic and comparisons:

.. code-block:: js

   x + y

   x >= (y - z)

Boolean expressions are automatically lifted to the formulae
level. Here's an expression (``x > y``) which is lifted into a formula
(``next ...``):

.. code-block:: js

   next (x > y)

Selectors
---------

A selector is a CSS selector written inside backticks, which evaluates
to a list of elements. For example, the following selector evaluates
to a list of all button elements in a given state:

.. code-block:: js

   `button`

We can iterate over the list of elements and refer to attributes,
properties, CSS styles, and more:

.. code-block:: js

   for b in `button` { b.textContent }

The example above evaluates to a list of strings.

If we're only interested in the first element, we can directly select
from it, as long as the list is not empty:

.. code-block:: js

   `button`.textContent

A selector's evaluated is state-dependent, meaning that it can
evaluate to different lists of elements depending on in which state it
is evaluated.

In the following example, we might not get the same list for both
selectors, so it could be true:

.. code-block:: js

   length(`button`) == 1 && next (length(`button`) == 2)

Elements
--------

The elements we get by evaluating selectors are objects. We can refer
to various things in those objects to read relevant state from the DOM:

``enabled``
   is the element enabled?
``visible``
   is the element visible?
``interactable``
   is the element interactable (e.g. clickable)?
``active``
   is the element active?
``classList``
   a list of strings, based on the ``class`` attribute
``css``
   a nested object with computed styles
``attributes``
   a nested object with HTML element attributes

As a fallback, any other key is evaluated as a property on the
corresponding runtime object of the element.

The Quickstrom expression ```button`.textContent`` corresponds to the
following JavaScript expression:

.. code-block:: js

   document.querySelector("button").textContent

Let Bindings
------------

In expressions and in formulae, we bind values to names using ``let``.
The general form is:

.. code-block:: js

   let name = expression; body

If we need many bindings, we can put them on separate lines:

.. code-block:: js

   let foo = 1;
   let bar = 2;
   let baz = 3;
   ...

Let is also supported as a top-level construct in source files.

Lazy Bindings
-------------

When expressions in let bindings are state-dependent, like those
involving selectors, we don't want the expression to be evaluated when
bound. Instead we annotate the binding using a tilde prefix, meaning
it's a lazy binding:

.. code-block:: js

   let ~myButtons = `.btn`;

The expression, in this case ```.btn```, is evaluated when another
expression refers to ``myButtons`` and is itself evaluated. Different
evaluations of ``myButtons`` may result in different values, depending
in which state the evaluation occurs. For example, the formula ``next
myButtons`` might not be equivalent to ``next (next myButtons)``.


Propositions
------------

When testing web apps using Quickstrom, we define *propositions* and
ask Quickstrom to check them for us. A proposition is a formula defined
at the top level. Useful propositions are state-dependent, so they are
always bound lazily in practice.

Let's say we have some proposition bound to ``prop``. We could check
it like so:

.. code-block:: js

   import quickstrom;

   let ~prop = ...;

   check prop with * when loaded?;

Read more about the ``check`` statement in the `Check`_ section.

Temporal Operators
------------------

In Quickstrom specifications, there are a bunch of built-in temporal
operators:

* ``next``
* ``always``

* ``until``

There are also utility operators defined using the built-in temporal
operators:

``unchanged``

Let's go through the operators and utility functions provided by
Quickstrom with some more examples!

Next
~~~~

The formula ``next p`` says that the formula ``p`` is true in the next
state.

But which state is "the next state"? It depends on which is the
current state. Temporal operators are always in relation to the
current state.

Always
~~~~~~

The formula ``always p`` says that the formula ``p`` is true in the
current and all subsequent states.

As an example, in the following proposition we check that the heading
is always ``"Home"``:

.. code-block:: js

   let ~title = `h1`.textContent;
   let ~prop = always (title == "Home");

Until
~~~~~

The formula ``p until q`` says that the formula ``p`` is true at least
until the formula ``q`` is true.

.. note::  

     * It doesn't matter if ``p`` is true or false once ``q`` is true. If we wanted that kind of exclusiveness, we could say ``p until (q && not p)``.
     * ``q`` can be true in the current state, in which case ``p`` never has to be true.
     * ``q`` only has to be true in one state, it doesn't have to stay true forever. If we want it true forever, we could say ``p until (always q)``.

In the following example, we check that a loading indicator is shown
until the page title is set correctly:

.. code-block:: js

   let ~title = `h1`.textContent;
   let ~loading = `.loading`.textContent;
   let ~prop = (loading == "Loading...") until (title == "Home");

Unchanged
~~~~~~~~~

The formula ``unchanged p`` says that ``p`` in the current state is
equal to ``p`` in the next state. Or in other words, that ``p`` doesn't
change from this state to the next.

This operator is useful when expressing state transitions, specifying
that a certain queried value should be the same both before and after
a particular transition. 

For instance, let's say we have a bunch of top-level definitions, all
based on DOM queries, describing a user profile:

.. code-block:: js

   let ~userName = ...;

   let ~userProfileUrl = ...;

We can say that the user profile information should not change in a
transition ``t`` by passing an array of those values:

.. code-block:: js

   let ~t = unchanged [userName, userProfileUrl]
       && ... // actual changes in transition
       ;

Actions
-------

We must instruct Quickstrom what actions are allowed. Actions are declared
at the top level using the ``action`` keyword. 

.. code-block:: js

   action launchTheMissiles! = click!(`#launch`);

By convention, actions are suffixed with an exclamation mark. Events
on the other hand are suffixed with a question mark, but still declared
using the ``action`` keyword:

.. code-block:: js

   action launched? = changed?(`#launch-status`)
     when `#launch-status`.textContent == "Launched!";

Built-in Actions
~~~~~~~~~~~~~~~~

The following actions and events are provided in `the Quickstrom
library <https://github.com/quickstrom/quickstrom/blob/{GIT_REF}/ulib/quickstrom.strom>`_:

``click!(elements)``
  Returns a list of click actions on the given elements.
``doubleClick!(elements)``
  Returns a list of double-click actions on the given elements.
``select!(elements)``
  Returns a list of select actions on the given option elements.
``clear!(elements)``
  Returns a list of clear actions on the given input elements.
``focus!(elements)``
  Returns a list of focus actions on the given input elements.
``keyPress!(key)``
  An action that presses the given key. Keys are defined by Quickstrom in the `keys <https://github.com/quickstrom/quickstrom/blob/{GIT_REF}/ulib/quickstrom/keys.strom>`_ object, e.g. ``keys.escape``.
``enterText!(text``
  An action that enters the given text into the currently focused input element.
``enterTextInto!(text, elements)``
  Returns a list of actions which enter the given text into the given input elements.
``noop!``
  An action that does nothing. Use together with ``timeout`` to wait for a certain amount of time, or for an event to occur within that timespan.
``refresh!``
  Refreshes the page.
``scrollBy!(x, y)``
  Scroll by absolute number of pixels from the current position. Use negative ``x`` or ``y`` arguments to scroll left or up.
``scrollByViewport!(x, y)``
  Scroll by ratio of the viewport size, i.e. ``scrollByViewport!(0, 1)`` to scroll down one viewport height. Use negative ``x`` or ``y`` arguments to scroll left or up.
``changed?``
  Emitted when the given element is visible.
``loaded?``
  Emitted when the document has loaded, i.e. the DOM ``load`` event.

.. note::

   Support for more actions should be added.

Action Preconditions
~~~~~~~~~~~~~~~~~~~~

Actions can be constraint to only be applicable under certain
preconditions. We use the `when` construct to express a precondition:

.. code-block:: js

   action launchTheMissiles! = click!(`#launch`) when canLaunch;

Many of the built-in actions in Quickstrom already have useful
preconditions set, like `click!` only be applicable on elements that
are visible, clickable, and enabled. This means that we don't have to specify
such basic preconditions. It's more likely that preconditions will be
domain-specific rules, if required at all.

Event Postconditions
~~~~~~~~~~~~~~~~~~~~

Similar to action preconditions are event postconditions. They are
used to declare an event that is only valid under certain
conditions.

For instance, a DOM element might have changed, but only if it's
changed in a certain way we considered it a specific event. The
`launched?` event we saw earlier is defined using a postcondition:

.. code-block:: js

   action launched? = changed?(`#launch-status`)
     when `#launch-status`.textContent == "Launched!";

Check
-----

We need to tell Quickstrom how to check a web application against a
specification. We do that using the ``check`` statement, which has
this general form:

.. code-block:: js

   check <props> with <actions> when <initial event>;

The placeholders work this way:


``<props>``
   This is wildcard matcher on all bindings in the current file. You can
   either literally refer to the propositions you want (e.g. ``prop``), or
   use a star to match against multiple propositions (e.g. ``prop_*``).
``<actions>``
   Also a wildcard matcher, matching on actions declared in the current file.
   In many cases this is just ``*``.
``<initial event>``
   The name of the initial event. The checker waits until this event occurs
   before it starts performing actions. In many web applications scenarios
   it will be ``loaded?``, but it might also be something more specialized.

As an example, we might end a specification with the following statement:

.. code-block:: js

   check prop* with * when loaded?;

State Machine Propositions
--------------------------

A powerful way of writing specifications is by expressing them as
state machines. A transition is expressed as an assertion about the
current state and another assertion about the next state. The state
machine proposition says that one of the transitions are always taken.

Here's an example based on a simple website with three pages:

.. code-block:: js

   let ~title = `h1`.textContent;

   // Transitions

   let ~goToAbout = title == "Home" && next title == "About";

   let ~goToContact = title == "Home" && next title == "Contact";

   let ~goHome = title != "Home" && next title == "Home";

   // Proposition

   let ~prop = always (goToAbout || goToContact || goHome);

The ``goToAbout``, ``goToContact``, and ``goHome`` transitions specify
how the title of the page changes, and the ``prop`` thus describes the
system as a state machine. It can be visualized as follows:

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

Source Files
------------

Specifications are written in source files with a ``.strom`` file
extension. A file is a *module*, and the module name is the filename
without the ``.strom.`` extension.

Other modules can be imported using the ``import <module name>;``
syntax at the top of a module. For instance, if we have a file
``foo.strom`` with the following contents:

.. code-block:: js
    :caption: foo.strom

    let x = 1;

We can import the ``foo`` module from the ``bar`` module and refer to
its bindings and actions:

.. code-block:: js
    :caption: bar.strom

    import foo;

    let y = x + 1;

The module system is very rudimentary. It works similarly as C header
files with include guards. Bindings from transitively imported modules
are also available. Continuing on the example above, if a third module
imported the ``bar`` module, the ``x`` binding would also be in scope
in ``baz``.

Include Paths
~~~~~~~~~~~~~

Quickstrom has a list of *include paths*, i.e. directories in which it
tries to find the files corresponding to imported modules. The current
working directory is implicitly an include path.
