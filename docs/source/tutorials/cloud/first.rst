Writing Your First Specification
================================

In this tutorial we'll specify and check an *audio player* web
application using Quickstrom Cloud.

Begin by signing in at `Quickstrom Cloud <app.quickstrom.io>`_.

Accessing the Audio Player
--------------------------

The web application we're going to test is already written and
available on GitHub. We're going to access it directly using
`htmlpreview.github.io`. Try opening the following link:

https://htmlpreview.github.io/?https://github.com/quickstrom/quickstrom/blob/main/examples/AudioPlayer.html 

That's the web application we're going to test. We're now ready to
write our specification.

A Minimal Specification
-----------------------

We'll begin by writing a specification that always makes the tests
pass:

#. Create a new specification by clicking *Specifications* in the top
   navigation bar, and then click the *New* button
#. Give the specification a name, like "audio-player"
#. Delete the existing code and replace it with the following:

   .. code-block:: haskell
      :linenos:

      module AudioPlayer where
   
      import Quickstrom
      import Data.Maybe (Maybe(..))
   
      readyWhen :: Selector
      readyWhen = ".audio-player"
   
      actions :: Actions
      actions = clicks
   
      proposition :: Boolean
      proposition = true

#. Click the *Create* button

A bunch of things are going on in this specification. Let's break it
down line by line:

* **Line 1:** We declare the ``AudioPlayer`` module. We must have a
  module declaration, but it can be named whatever we like.
* **Line 3-4:** We import the Quickstrom module. This is where we find
  definitions for DOM queries, actions, and logic. We also import
  `Maybe` which we'll need later on.
* **Line 6-7:** The ``readyWhen`` definitions tells Quickstrom to wait
  until there's an element in the DOM that matches this CSS
  selector. After this condition holds, Quickstrom will start
  performing actions. We use ``.audio-player`` as the selector, which
  is used as a class for the top-level ``div`` in the audio player
  web application.
* **Line 9-10:** Our ``actions`` specify what Quickstrom should try to do. In
  this case, we want it to click any available links, buttons, and so
  on.
* **Line 12-13:** In the ``proposition``, we specify what it means for
  the system under test to be valid. For now, we'll set it to
  ``true``, meaning that *any* behavior is considered valid.

Running Tests
-------------

Let's run some tests:

#. Create a new check configuration by clicking *New* in the
   *Configurations* section
#. Give it a name, like "htmlpreview"
#. Use the following URL as the origin:

   https://htmlpreview.github.io/?https://github.com/quickstrom/quickstrom/blob/main/examples/AudioPlayer.html

#. Click the *Create* button
#. Find your newly created configuration in the table and click *Check*

This schedules a new check and opens the check view. It updates live
as the check makes progress. After some time, you should see log
output like the following:

.. code::
   
   Running 10 tests...
   
   ―――――――――――――――――――――――――――
   
   10 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   20 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   30 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   40 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   50 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   60 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   70 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   80 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   90 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   100 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   
   Passed 10 tests.


Cool, we have it running! So far, though, we haven't done much
testing. Quickstrom is happily clicking its way around the web
application, but whatever it finds we say "it's all good!" Let's make
our specification actually say something about the audio player's
intended behavior.

Refining the Proposition
------------------------

Our system under test, the audio player, is very simple. There's a
button for playing or pausing the audio player, and there's a time
display.

Our specification will describe how the player should
work. Informally, we state the requirements as follows:

* Initially, the player should be ``paused``
* When ``paused``, and when the play/pause button is clicked, it
  should transition to the ``playing`` state
* When in the ``playing`` state, the time display should reflect the
  progress with a ticking minutes and seconds display
* When ``playing``, and when the play/pause button is clicked, it should
  go to the ``paused`` state
* In the ``paused`` state, the button should say "Play"
* In the ``playing`` state, the button should say "Pause"

Let's translate those requirements to a formal specification in
Quickstrom. Go back to your specification (you'll find it under
*Specifications* in the top navigation bar), and click the *Edit*
button.

Now it's time to edit the specification code. Begin by defining two
helpers, extracting the text content of the time display and the
play/pause button. Place these definitions at the bottom of
``AudioPlayer.spec.purs``:

.. code-block:: haskell

   timeDisplayText :: Maybe String
   timeDisplayText =
     map _.textContent (queryOne ".time-display" { textContent })

   buttonText :: Maybe String
   buttonText =
     map _.textContent (queryOne ".play-pause" { textContent })

Next, we'll change the ``proposition``. Remove ``true`` and type in
the following code:

.. code-block:: haskell

   proposition :: Boolean
   proposition =
     let
       playing = ?playing
   
       paused = ?paused
   
       play = ?play
   
       pause = ?pause
   
       tick = ?tick
     in
       paused && always (play || pause || tick)

All those terms prefixed with question marks are called *holes*. A
hole is a part of a program that is yet to be written, like a
placeholder. We'll fill the holes one by one.

The last line in our proposition can be read in English as:

    Initially, the record player is paused. From that point, one can
    either play or pause, or the time can tick while playing, all
    indefinitely.

OK, onto filling the holes!

Filling Holes in the Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's start with the definitions that describe *states* that the
program can be in.

The ``playing`` definition should describe what it means to be in the
``playing`` state. We specify it by stating that the button text
should be "Pause". Replace ``?playing`` with the following expression:

.. code-block:: haskell

   buttonText == Just "Pause"

The ``Just "Pause"`` means that there is a matching element with text
content "Pause". ``Nothing`` would mean that the query didn't find any
element.

Similary, the ``paused`` state is defined as the button text being
"Play". Replace ``?paused`` with:

.. code-block:: haskell

   buttonText == Just "Play"

We've now specified the two states that the audio player can be
in. Next, we specify *transitions* between states.



The definition ``play`` describes a transition between ``paused`` and
``playing``. Replace the hole ``?play`` with the following expression:

.. code-block:: haskell

   paused && next playing

OK, so what's going on here? We specify that the current state is
``paused``, and that the next state is ``playing``. That's how we
encode state transitions.

The ``pause`` transition is similar. Replace ``?pause`` with the
following expression:

.. code-block:: haskell

   playing && next paused

Finally, we have the ``tick``. When we're in the ``playing`` state,
the time display changes its text on a ``tick``. The displayed time
should be monotonically increasing, so we compare alphabetically the
current and the next time.

Replace the hole ``?tick`` with the following expression:

.. code-block:: haskell

   playing
     && next playing
     && timeDisplayText < next timeDisplayText

If the time display would go past "99:59", we'd get into trouble with
this specification. But because we won't run tests for that long, we
can get away with the string comparison.

That's it! We've filled all the holes. Your proposition should now
look something like this:

.. code-block:: haskell

   proposition :: Boolean
   proposition =
     let
       playing = buttonText == Just "Pause"
   
       paused = buttonText == Just "Play"
   
       play = paused && next playing
   
       pause = playing && next paused
   
       tick =
         playing
           && next playing
           && timeDisplayText < next timeDisplayText
     in
       paused && always (play || pause || tick)


Let's run some more tests.

Catching a Bug
--------------

Schedule a new check, now that we've fleshed out the specification.

You'll see a bunch of output, involving shrinking tests and more. It
should end with something like the following:

.. code-block::
   :emphasize-lines: 16
   
   1. State
     • .play-pause
         -
            - property "textContent" = "Play"
     • .time-display
         -
            - property "textContent" = "00:00"
   2. click button[0]
   3. click button[0]
   4. State
     • .play-pause
         -
            - property "textContent" = "Play"
     • .time-display
         -
            - property "textContent" = "NaN:NaN"
   
   Failed after 1 tests and 4 levels of shrinking.


Whoops, look at that! It says that the time display shows
"NaN:NaN". We've found our first bug using Quickstrom!

There's another version of the web application with a fix in place for
this bug. Create a new check configuration but using the following URL
as the origin:

https://htmlpreview.github.io/?https://github.com/quickstrom/quickstrom/blob/main/examples/AudioPlayer.fix-1.html

Check again but with your new configuration. All tests pass!

Are we done? Is the audio player correct? Not quite.

Transitions Based on Time
-------------------------

The audio player transitions between states mainly as a result of
user action, but not only. A ``tick`` transition (going from
``playing`` to ``playing`` with an incremented progress) is triggered
by *time*.

We'll try tweaking Quickstrom's options related to :doc:`trailing
state changes <../../topics/trailing-state-changes>` to test more of the
time-related behavior of the application.

Create a new check configuration with the same origin URL as the previously created one, but open up the *Advanced options* section and set *Max trailing state changes* to ``1`` rather than ``0``.

You should see output such as the following:

.. code::
   
   1. State
     • .play-pause
         -
            - property "textContent" = "Play"
     • .time-display
         -
            - property "textContent" = "00:00"
   2. click button[0]
   3. State
     • .play-pause
         -
            - property "textContent" = "Play"
     • .time-display
         -
            - property "textContent" = "00:01"

   Failed after 1 tests and 5 levels of shrinking.

Look, another bug! It seems that there are ``tick`` transitions even
though the play/pause button indicates that we're in the ``paused``
state.

In fact, the problem is the button text, not the time display. There's
another version at the following URL with another bug fix:

https://htmlpreview.github.io/?https://github.com/quickstrom/quickstrom/blob/main/examples/AudioPlayer.fix-2.html

Create yet another check configuration with this origin URL, and you
should have all tests pass.

Summary
-------

Congratulations! You've completed the tutorial, created your first
specification, and found multiple bugs.

Have we found all bugs? Possibly not. This is the thing with testing.
We can't know if we've found all problems. However, Quickstrom tries
very hard to find more of them for you, requiring less effort.

This tutorial is intentionally fast-paced and low on theory. Now that
you've got your hands dirty, it's a good time to check out
:doc:`../../topics/specification-language` to learn more about the
operators in Quickstrom.
