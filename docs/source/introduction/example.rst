Example: Record Player
======================

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