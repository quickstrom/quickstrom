Trailing State Changes
======================

By default, Quickstrom only listens for a single DOM state change
after each action it performs. This behavior can be overriden, so that
it waits for a configurable number of *trailing state changes*.

The term *trailing* refers primarily to asynchronous changes that
occur as result of an action. For example:

* the user agent clicks a button
* a loading indicator is shown immediately
* an HTTP request is performed
* later, the result of the request is printed

In this example, the loading indicator being shown is the first state
change. The result of the HTTP request being shown is the trailing
state change.

Some systems change the state of the DOM without any dependence on
user action, and do so infinitely. For instance, a clock (hopefully)
keeps ticking, no matter what the user is up to. It doesn't make much
sense to think of a clock's behavior as "trailing". However, it's
still possible to test a finite subsequence of such a behavior using
Quickstrom and trailing state changes.

Command-Line Options
--------------------

The command-line options available are:

* ``--max-trailing-state-changes=NUMBER``: how many trailing state
  changes Quickstrom will try to observe.
* ``--trailing-state-change-timeout=MILLISECONDS``: maximum time
  that it will wait for a change of DOM state. The timeout doubles
  for every subsequent trailing state change that is awaited.

Let's say we set the following options:

.. code-block:: shell

   --max-trailing-state-changes=3
   --trailing-state-change-timeout=200

Then the DOM state changes would be observed at the following times:

1. initial state, immediately
2. first trailing state, at most 200ms after #1
3. second trailing state, at most 400ms after #2
4. third trailing state, at most 800ms after #3

It's *at most*, because Quickstrom observes the DOM and can pick up
state changes as they happen.
