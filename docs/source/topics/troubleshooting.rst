Troubleshooting
===============

This documents collects some common problems and tips on how to
identify what's not working correctly.

If you're troubleshooting a failing Quickstrom check, make sure to
enable debug logs:

.. code-block:: console

   $ quickstrom check --log-level=DEBUG ...
 
No WebDriver Session
--------------------

If you get the following error when using GeckoDriver (especially after
having successfully run before):

.. code-block:: shell

   quickstrom: user error (E NoSession)

It’s probably because the WebDriver package in Quickstrom failed to
clean up its session. This is a known bug. To work around it, restart
Geckodriver and rerun your Quickstrom command.
