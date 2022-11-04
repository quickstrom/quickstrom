Writing Your First Specification
================================

In this tutorial we'll specify and check an *audio player* web
application using the free version of Quickstrom.

The tutorial assumes you're running on a Unix-like operating system
and that you have Docker installed. You may run this using :doc:`other
installation methods <../../installation>` for Quickstrom and your
WebDriver server, but all commands in this document are using Docker.

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

Finally, we need a WebDriver server. Pull that down with Docker, too:

.. code-block:: console

   $ docker pull selenium/standalone-chrome:3.141.59-20200826

Downloading the Audio Player
-----------------------------

The web application we're going to test is already written. Download
it using ``curl``:

.. code-block:: console
                
   $ curl -L https://github.com/quickstrom/quickstrom/raw/main/examples/AudioPlayer.html -o AudioPlayer.html


If you don't have ``curl`` installed, you can download it from `this
URL
<https://github.com/quickstrom/quickstrom/raw/main/examples/AudioPlayer.html>`__
using your web browser. Make sure you've saved it our working
directory as ``AudioPlayer.html``.

.. code-block:: console
                
   $ ls
   AudioPlayer.html

OK! We're now ready to write our specification.

A Minimal Specification
-----------------------

We'll begin by writing a specification that always makes the tests
pass. Create a new file ``AudioPlayer.spec.purs`` and open it in your
text editor of choice:

.. code-block:: console
                
   $ touch AudioPlayer.spec.purs
   $ $EDITOR AudioPlayer.spec.purs

Type in the following in the file and save it:

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

Let's run some tests!

First, we need a Docker network. Let's name it ``quickstrom``:

.. code-block:: console

   $ docker network create quickstrom

Next, from within your ``my-first-spec`` directory, launch a ChromeDriver instance in the
background:

.. code-block:: console

   $ docker run --rm -d \
       --network quickstrom \
       --name webdriver \
       -v /dev/shm:/dev/shm \
       -v $PWD:/my-first-spec \
       selenium/standalone-chrome:3.141.59-20200826

Notice how we mount the current working directory to
``/my-first-spec`` in the container. We do this to let Chrome access
the ``AudioPlayer.html`` file.

Now, let's launch Quickstrom, again from within your ``my-first-spec`` directory:

.. code-block:: console

   $ docker run --rm \
     --network quickstrom \
     -v $PWD:/my-first-spec \
     quickstrom/quickstrom \
     quickstrom check \
     --webdriver-host=webdriver \
     --webdriver-path=/wd/hub \
     --browser=chrome \
     --tests=5 \
     /my-first-spec/AudioPlayer.spec.purs \
     /my-first-spec/AudioPlayer.html

After some time, you should see an output like the following:

.. code::
   
   Running 5 tests...
   
   ―――――――――――――――――――――――――――
   
   20 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   40 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   60 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   80 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   100 Actions
   Test passed!
   
   ―――――――――――――――――――――――――――
   
   
   Passed 5 tests.


Cool, we have it running! So far, though, we haven't done much
testing. Quickstrom is happily clicking its way around the web
application, but whatever it finds we say "it's all good!" Let's make
our specification actually say something about the audio player's
intended behavior.

Refining the Proposition
------------------------

Our system under test (``AudioPlayer.html``) is very simple. There's
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
  go to the ``paused`` state
* In the ``paused`` state, the button should say "Play"
* In the ``playing`` state, the button should say "Pause"

Let's translate those requirements to a formal specification in
Quickstrom.

Begin by defining two helpers, extracting the text content of the time
display and the play/pause button. Place these definitions at the
bottom of ``AudioPlayer.spec.purs``:

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

Run Quickstrom again, now that we've fleshed out the specification:

.. code-block:: console

   $ docker run --rm \
     --network quickstrom \
     -v $PWD:/my-first-spec \
     quickstrom/quickstrom \
     quickstrom check \
     --webdriver-host=webdriver \
     --webdriver-path=/wd/hub \
     --browser=chrome \
     --tests=5 \
     /my-first-spec/AudioPlayer.spec.purs \
     /my-first-spec/AudioPlayer.html

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
