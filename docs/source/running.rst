Running
=======

To check a specification, you must have a running `geckodriver
<https://github.com/mozilla/geckodriver>`__ instance. If you've installed
Quickstrom using Nix, you can get geckodriver through Nix, too. Run this in a
separate tab in your terminal:

.. code:: console

   nix-shell -p geckodriver --run geckodriver

Next, run ``quickstrom check`` and supply the path to the specification
file along with the origin URL (can also be a local file path).

.. code:: console

   quickstrom check /path/to/my/specification http://example.com

For instance, you can check the TodoMVC React implementation:

.. code:: console

   quickstrom check \
      specs/other/TodoMVC.spec.purs \
      http://todomvc.com/examples/react

.. note:: 

   Running tests can take a lot of time, especially if there’s a failure and
   Quickstrom tries to shrink to the minimal failing behavior. Optimizations
   are due, but for now it’s pretty slow.

No WebDriver Session
--------------------

If you get the following error:

.. code:: console

   quickstrom: user error (E NoSession)

It’s probably because the WebDriver package in Quickstrom failed to
clean up its session. This is a known bug. To work around it, restart
Geckodriver:

.. code:: console

   geckodriver

And rerun your Quickstrom command.

Help
----

There are various flags and options for the ``quickstrom`` executable.
Run with ``--help`` to learn more.
