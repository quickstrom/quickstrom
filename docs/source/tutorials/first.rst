Writing Your First Specification
================================

In this tutorial we'll specify and check an *audio player* web
application using Quickstrom.

The tutorial assumes you're running on a Unix-like operating system
and that you have Docker installed. You may run this using :doc:`other
installation methods <../../installation>`, but all commands in this
document are using Docker.

Open up a terminal and create a new directory to work in:

.. code-block:: console

   $ mkdir my-first-spec
   $ cd my-first-spec

Installing with Docker
----------------------

In this tutorial you need a working installation of Docker. Head over
to `docker.com <https://www.docker.com/>`__ and set it up if you
haven't already.

Next, pull the QuickStrom image using Docker:

.. code-block:: console

   $ docker pull quickstrom/quickstrom:latest

Downloading the Audio Player
-----------------------------

The web application we're going to test is already written. Download
it using ``curl``:

.. code-block:: console
                
   $ curl -L https://github.com/quickstrom/quickstrom/raw/main/examples/audioplayer.html -o audioplayer.html


If you don't have ``curl`` installed, you can download it from `this
URL
<https://github.com/quickstrom/quickstrom/raw/main/examples/AudioPlayer.html>`__
using your web browser. Make sure you've saved it our working
directory as ``AudioPlayer.html``.

.. code-block:: console
                
   $ ls
   audioplayer.html

OK! We're now ready to write our specification.

A Minimal Specification
-----------------------

We'll begin by writing a specification that always makes the check
pass. Create a new file ``audioplayer.strom`` and open it in your text
editor of choice:

.. code-block:: console
                
   $ touch audioplayer.strom
   $ $EDITOR audioplayer.strom

Type in the following in the file and save it:

.. code-block:: javascript
   :linenos:

   import quickstrom;

   action ~playOrPause! = click!(`.play-pause`);

   let ~proposition = true;

   check proposition with * when loaded?;

A bunch of things are going on in this specification. Let's break it
down line by line:

* **Line 1:** We import the Quickstrom module. This is where we find
  definitions for DOM queries, actions, and logic. We also import
  `Maybe` which we'll need later on.
* **Line 3:** Our actions specify what Quickstrom should try to do. In
  this case, we want it to click the play/pause button.
* **Line 5:** In the ``proposition``, we specify what it means for
  the system under test to be valid. For now, we'll set it to
  ``true``, meaning that we require only a single state, and that
  *any* such state is considered valid.
* **Line 7:** The ``check`` statement tells Quickstrom how to test our
  application. We ask it to check our defined ``proposition``, with
  all declared actions, once the ``loaded?`` event has occured.

Running a Test
--------------

Let's run some tests! Launch Quickstrom from within your
``my-first-spec`` directory:

.. code-block:: console

   $ docker run --rm \
     -v $PWD:/my-first-spec \
     quickstrom/quickstrom:0.5.0 \
     quickstrom -I/my-first-spec check \
     audioplayer \
     /my-first-spec/audioplayer.html \
     --browser=chrome

You should see output like the following:

.. code::
   
   The test passed.


Cool, we have it running! So far, though, we haven't done much
testing. Quickstrom doesn't do more than the specification requires,
and right now any initial state is good enough, so it doesn't perform
any actions. Let's make our specification say something about the
audio player's intended behavior.

Refining the Proposition
------------------------

Our system under test (``audioplayer.html``) is very simple. There's
a button for playing or pausing the audio player, and there's a time
display.

Our specification will describe how the player should
work. Informally, we state the requirements as follows:

* Initially, the player should be ``paused``
* When ``paused``, and when the play/pause button is clicked, it
  should transition to the ``playing`` state
* When in the ``playing`` state, the time display should reflect the
  progress with a ticking minutes and seconds display
* When ``playing``, and when the play/pause button is clicked, it should
  go to the ``paused`` state, and time should not change
* In the ``paused`` state, the button should say "Play"
* In the ``playing`` state, the button should say "Pause"

Let's translate those requirements to a formal specification in
Quickstrom.

Begin by defining two element helpers, extracting the text content of
the play/pause button, and extracing and parsing the time display
text. The time is represented as total number of seconds in our
specification, making it easier to compare.

Place these just after the imports section in ``audioplayer.strom``:

.. code-block:: javascript

   let ~buttonText = `.play-pause`.textContent;
   
   let ~timeInSeconds =
     let [minutes, seconds] = split(":", `.time-display`.textContent);
     parseInt(minutes) * 60 + parseInt(seconds);

Next, we'll define the two states as booleans:

.. code-block:: javascript

   let ~playing = buttonText == "Pause";
   
   let ~paused = buttonText == "Play";

We also need to declare the actions a bit more precisely. Change to
existing action declartion to the following:

.. code-block:: javascript

   action ~pause! = click!(`.play-pause`) when playing;

   action ~play! = click!(`.play-pause`) timeout 1000 when paused;

Finally, we'll change the ``proposition``. Remove ``true`` and type in
the following code:

.. code-block:: javascript

   let ~proposition =
     let ~play = ...;
     let ~pause = ...;
     let ~tick = ...;
     paused && (always {20} (play || pause || tick));

.. note::

   The ``...`` parts aren't valid ecpressions, but we'll replace them with valid ones in the next section.

The last line in our proposition can be read in English as:

    Initially, the record player is paused. From that point, one can
    either play or pause, or the time can tick while playing, all
    indefinitely.

OK, onto adding the missing parts!

The Missing State Transitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have a bunch of ``...`` placeholders in our state transition
formulae. Let's fill them in!

The definition ``play`` describes a transition between
``paused`` and ``playing``:

.. code-block:: javascript

   let ~play =
     paused
       && nextT playing
       && unchanged(timeInSeconds);

OK, so what's going on here? We specify that the current state is
``paused``, and that the next state is ``playing``. That's how we
encode state transitions. We also say that the time shouldn't change.

.. note::

   We need to use ``nextT`` instead of ``next`` here, because we don't
   want to force another state being read. If there is a next state
   available, we say that it should be ``playing``, otherwise we
   default to true. That's what the ``T`` in ``nextT`` means.

The ``pause`` transition should look similar:

.. code-block:: javascript

   let ~pause =
     playing
       && nextT paused
       && unchanged(timeInSeconds);

Finally, we have the ``tick``. When we're in the ``playing`` state,
the time changes on a ``tick``. The time should be monotonically
increasing, so we compare the current and the next time:

.. code-block:: javascript

   let ~tick =
     playing
       && nextT playing
       && (let old = timeInSeconds; nextT (old < timeInSeconds));

That's it! Your proposition should now look something like this:

.. code-block:: javascript

   proposition :: Boolean
   proposition =
     let
       play = paused && next playing
   
       pause = playing && next paused
   
       tick =
         playing
           && next playing
           && timeInSeconds < next timeDisplayText
     in
       paused && always (play || pause || tick)


Let's run some more tests.

Catching a Bug
--------------

Run Quickstrom again, now that we've fleshed out the specification:

.. code-block:: console

   $ docker run --rm \
     -v $PWD:/my-first-spec \
     quickstrom/quickstrom:0.5.0 \
     quickstrom -I/my-first-spec check \
     audioplayer \
     /my-first-spec/audioplayer.html \
     --browser=chrome

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

Open up ``AudioPlayer.html``, and change the following lines near the
end of the file:

.. code-block:: javascript

   case "pause":
       return await inPaused();

They should be:

.. code-block:: javascript

   case "pause":
       return await inPaused(time); // <-- this is where we must pass in time

Rerun the tests using the same ``quickstrom`` command as before. All
tests pass!

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

Run new tests by executing the following command:

.. code-block:: console
   :emphasize-lines: 10-11

   $ docker run --rm \
     --network quickstrom \
     -v $PWD:/my-first-spec \
     quickstrom/quickstrom \
     quickstrom check \
     --webdriver-host=webdriver \
     --webdriver-path=/wd/hub \
     --browser=chrome \
     --tests=5 \
     --max-trailing-state-changes=1 \
     --trailing-state-change-timeout=500 \
     /my-first-spec/AudioPlayer.spec.purs \
     /my-first-spec/AudioPlayer.html

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

In fact, the problem is the button text, not the time display. I'll
leave it up to you to find the error in the code, fix it, and make
the tests pass.

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
