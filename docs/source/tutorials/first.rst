Writing Your First Specification
================================

The following specifies a record player, featuring a button that toggles
between the paused and playing states. Let's say the HTML for the record
player looks something like the following:

.. code-block:: html

   <div class="record-player">
     ...
     <nav>
        <button class="play-pause">Play</button>
     </nav>
   </div>

The specification waits for a DOM element matching the CSS selector
``.record-player`` before taking any action.

.. code-block:: haskell

   readyWhen :: Selector
   readyWhen = ".record-player"

This helper definition finds an optional text for the play/pause button.

.. code-block:: haskell

   buttonText :: Maybe String
   buttonText =
     map _.textContent (queryOne ".play-pause" { textContent })

Based on the specified actions, Quickstrom generates click actions for all
clickable elements:

.. code-block:: haskell

   actions :: Actions
   actions = clicks

The proposition describes the correct behavior of the web application.
Here we start in the paused state, and a valid transition is either play
or pause.

.. code-block:: haskell

   proposition :: Boolean
   proposition =
     let
       playing = buttonText == Just "Pause"
   
       paused = buttonText == Just "Play"
   
       play = paused && next playing
   
       pause = playing && next paused
     in
       paused && always (play || pause)

The last line, ``paused && always (play || pause)``,
can be read in English as:

    Initially, the record player is paused, and from there on one can either
    play or pause, indefinitely.

Now, let’s run Quickstrom with a broken implementation of the record
player. We get a minimal behavior that violates the specification:

.. code-block:: console

   $ quickstrom check examples/RecordPlayer.spec.purs examples/RecordPlayer.html
   Running 10 tests...
   Test failed. Shrinking...

   1. State
     • .play-pause
         -
            - property "textContent" = "Play"
   2. click button[0]
   3. State
     • .play-pause
         -
            - property "textContent" = "Pause"
   4. click button[0]
   5. State
     • .play-pause
         -
            - property "textContent" = ""

   Failed after 1 tests and 3 levels of shrinking.


Although not highlighted, the last item with the blank text is where
we have our problem. Looks like pausing broke the record player!

Try It Yourself
---------------

You'll find this complete specification and broken implementation in
the ``examples`` in the Quickstrom repository:

* `RecordPlayer.html <https://github.com/quickstrom/quickstrom/blob/main/examples/RecordPlayer.html>`__
* `RecordPlayer.spec.purs <https://github.com/quickstrom/quickstrom/blob/main/examples/RecordPlayer.spec.purs>`__

Download the files and check them yourself, after you've
:doc:`installed <../installation>` Quickstrom.
